module eval

open AST
open System.IO
open Template
open System


(* Takes a list of Expr items and categorizes them by type in order of Dates, Instances, Locations, and Actions *)
let rec sortHelp (ins: Expr list) (dates: Expr list) (names: Expr list) (locs: Expr list) (actions: Expr list) (instances: Expr list)=
  match ins with
  | [] -> 
      let result = [dates; instances; locs; actions]
      let listRes = List.concat result
      if List.length dates = 0 then             // If no dates, default to now and 1 week from now
        let defaultDateDT = DateTime.Now
        let defaultEndDT = defaultDateDT.AddDays(7)
        let defaultDate = string(defaultDateDT.Month) + "/" + string(defaultDateDT.Day) + "/" + string(defaultDateDT.Year) + " " + "00:00" |> Date
        let defaultEnd = string(defaultEndDT.Month) + "/" + string(defaultEndDT.Day) + "/" + string(defaultEndDT.Year) + " " + "00:00" |> Date
        let defaultDR = DateRange(defaultDate, defaultEnd)
        (listRes, actions, defaultDR, names)
      else
        (listRes, actions, dates[0], names)
  | e1::e2 ->
      match e1 with
      | DateRange (_,_) ->
        let newDates = e1::[]     // Overwrites existing dates
        sortHelp (e2) (newDates) (names) (locs) (actions) (instances)
      | Name (_) -> 
        let newNames = e1::names
        sortHelp (e2) (dates) (newNames) (locs) (actions) (instances)
      | Location (_,_) ->
        let newLocs = e1::locs
        sortHelp (e2) (dates) (names) (newLocs) (actions) (instances)
      | Action (_,_,_) ->
        let newActs = e1::actions
        sortHelp (e2) (dates) (names) (locs) (newActs) (instances)
      | Instance (_,_,_) ->
        let newInstances = e1::instances
        sortHelp (e2) (dates) (names) (locs) (actions) (newInstances)

(* Evaluator for Timescale types: takes a Timescale as input and returns a string *)
let evalTimescale (tf: Timescale) : string =
  match tf with
  | Minute -> "Minutes"
  | Hour -> "Hours"
  | Day -> "Days"
  | Week -> "Weeks"
  | Month -> "Months"
  | Year -> "Years"

(* Helper function for assignAct: takes as input a timescale, start date, and frequency int, and returns the next date
   [i] [timescale]s from start date*)
let nextDate(t: Timescale) (d: DateTime) (i: int) =
   match t with
   | Minute -> d.AddMinutes(i)
   | Hour -> d.AddHours(i)
   | Day -> d.AddDays(i)
   | Week -> 
       let daysToWeeks i = 7*i
       d.AddDays(daysToWeeks i)
   | Month -> d.AddMonths(i)
   | Year -> d.AddYears(i)

(* Helper function for assignAct: takes as input two dates, a timescale, and a frequency int and returns a list of the dates every
  [i] [timescales]s between the two input dates*)
let rec dateSeq (d1: DateTime) (d2: DateTime) (t: Timescale) (i: int)=
  let c = DateTime.Compare(d1, d2)
  if c < 0 then
    let next = nextDate t d1 i
    d1::(dateSeq next d2 t i)
  else
    []

(* Takes as input an action and a DateRange and returns a list of the dates on which the action must be performed *)
let assignAct (act: Expr) (dr: Expr): DateTime list =
  match (act, dr) with
  | (Action(n,i,t), DateRange(starting, ending)) ->
      let newStarting = DateTime.Parse(starting)
      let newEnding = DateTime.Parse(ending)
      let res = dateSeq newStarting newEnding t i
      List.sort res
  | _ -> failwith "Internal error: Mismatched type input while assigning dates to action"

(* Helper function that takes as input an action and a DateTime list for just that Action and builds a list of DateTime * Action list tuples *)
let rec dateActTupleBuilder (act: Expr) (dates: DateTime list) =
  match dates with
  | [] -> []
  | d::ds ->
      (d,[act])::(dateActTupleBuilder act ds)

(* Sorts a list of (DateTime * Action list) by date *)
let sortDateActionTupleList (tuples: (DateTime * Expr list) list) =
  List.sortBy (fun (a,_) -> a) tuples

(* Takes as input a list of (DateTime * Action list) and combines the Action lists of repeat dates *)
let rec combineDatesIntoList (dates: (DateTime*Expr list) list) =
  match dates with
  | [] -> []
  | (x,xs)::[] -> [(x,xs)]
  | (x,xs)::(y,ys)::z ->
      if x = y then
        let newList = (x,(xs@ys))::z    // Combine x and y tuples if same date
        combineDatesIntoList newList
      else
        let newList = (y,ys)::z
        (x,xs)::(combineDatesIntoList newList)

(* Helper function that takes as input a list of actions and a date range and returns a list of (DateTime * Action list) tuples that
   includes all actions and all dates for those actions *)
let rec assignManyActs (acts: Expr list) (dr: Expr) =
  match acts with
  | [] -> []
  | b::bs ->
    let dates = assignAct b dr
    let actTuples = dateActTupleBuilder b dates
    actTuples @ (assignManyActs bs dr) // Returns list of dates for this action plus recursive call for lists of dates for following actions
      |> sortDateActionTupleList
      |> combineDatesIntoList

(* Converts a list of Actions into a LaTeX string *)
let rec processActions (acts: Expr list) =
  match acts with
  | [] -> ""
  | b::bs ->
      match b with
      | Action(n,i,t) ->
        """\item """ + n + "\n" + (processActions bs)
      |_ -> failwith "Incorrect usage of this function"

(* Takes as input a list of (DateTime * Action list) tuples and returns a LaTeX string expressing the elements of a checklist *)
let rec generateSchedule (schedule: (DateTime * Expr list) list) =
  match schedule with
  | [] -> """\end{todolist}""" + "\n" + "Nothing more to do--thank you!"
  | (d,acts)::ss -> 
      "\n" + """\subsection*{""" + string(d.DayOfWeek) + ", " + string(d.Month) + "/" + string(d.Day) + "/" + string(d.Year) + ": " + string(d.TimeOfDay) + """}""" 
        + "\n" + (processActions acts) + (generateSchedule ss)

(* Takes as input a list of Actions and a DateRange and returns a LaTeX string expressing a checklist *)
let compileSchedule (acts: Expr list) (dr: Expr) =
  let schedule = assignManyActs acts dr
  let scheduleString = generateSchedule schedule
  """\begin{todolist}""" + "\n" + scheduleString 
  
(* Evaluator for Type types: takes a Type as input and returns a string *)
let evalCategory (thing: Category) : string =
  match thing with
    | Kid -> "Child"
    | Plant -> "Plant"
    | Dog -> "Dog"
    | Cat -> "Cat"
    | House -> "House"
    | Fish -> "Fish"
    | Adult -> "Adult"

(* Evaluator for Fields types: takes Fields as input and returns a string *)
let rec eval_fields (fields: Fields) : string =
  match fields.info with
  | [] -> "\n" + """\item """ + "No additional info about this item" + "\n"+ """\end{itemize} """
  | (s1,s2)::[] -> "\n" + """\item """ + s1 + ": " + s2 + "\n" + """\end{itemize} """
  | (s1,s2)::fs -> 
      "\n"+ """\item """ + s1 + ": " + s2 + "\n" + eval_fields {info = fs}

(* Evaluates all types of Exprs by converting to appropriate LaTeX strings *)
let eval_expr (e: Expr) =
  match e with
  | Instance (t, s, f) -> 
      """\item """ + "About " + s + ", " + evalCategory(t) + ":\n"+ 
        "\n"+ """\begin{itemize} """ + eval_fields(f) + "\n"
  | Location (s, ss) -> 
      """\item """ + "Location: " + s + "\n" + """\begin{itemize}""" + "\n"+
        eval_fields(ss) + "\n"
  | Action (s, i, t) ->
      """\item """ + "Task: " + s + " every " + string(i) + " " + evalTimescale(t) + "\n"
  | DateRange (a,b) -> ""
  | Name (s) -> ""

(* Evaluates a list of Exprs *)
let rec eval_list (e: Expr list) (sw_this: StreamWriter)=
  match e with
  | [] -> ""
  | e1::es -> 
      sw_this.WriteLine (eval_expr e1)
      eval_list es sw_this
      
(* Helper function to extract a start date and an end date from a DateRange object *)
let extractDates (dr: Expr) =
  match dr with
  | DateRange(d1,d2) -> (DateTime.Parse(d1),DateTime.Parse(d2))
  | _ -> failwith "Internal error: type mismatch"

(* Takes as input a list of Caretakers and converts to a string *)
let rec evalCaretakers (cs: Expr list) =
  match cs with
  | [] -> "Caretaker"
  | ct::[] -> 
    match ct with
    | Name(n) -> n
    | _ -> failwith "Not a caretaker"
  | ct::cts ->
    match ct with
    | Name(n) -> n + ", " + (evalCaretakers cts)
    | _ -> failwith "Not a caretaker"


(* Evaluates expr list *)
let eval (e: Expr list): string =
  // Define certain useful values
  let (new_e, actions, dates, caretakers) = sortHelp e [] [] [] [] []
  let (startDate, endDate) = extractDates(dates)


  // Create output LaTeX script
  let path = @"..\code\ScheduleDoc.tex"
  use sw = File.CreateText path
  sw.WriteLine (setup (evalCaretakers caretakers) startDate endDate)
  sw.WriteLine ("""\section{Things to Care For}""" + "\n")
  sw.WriteLine ("""\begin{itemize}""" + "\n")

  // Write lsit of instances, location, actions
  let res = eval_list new_e sw
  sw.WriteLine ("""\end{itemize}""" + "\n")
  
  // Set up schedule checklist
  let sched = compileSchedule actions dates
  let openSched = """\section{Schedule from """ + string(startDate) + " to " + string(endDate) + """}""" + "\n"
  sw.WriteLine openSched
  sw.WriteLine sched

  // Close LaTex script
  sw.WriteLine wrapup
  """LaTeX File ScheduleDoc.tex has been generated in current working directory.""" + "\n\n" + """Type "pdflatex ScheduleDoc.tex" at the command line to generate your PDF in your current working directory."""