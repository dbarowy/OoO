module eval

open AST
open System.IO
open Template

// NEED TO HAVE SOME SORT OF CATEGORIZING/SORTING FUNCTION
let sort (es: Expr list) : Expr list =
  match es with
  | [] -> []
  | e1::[] -> [e1]
  | e1::es -> failwith "to be implemented"

let evalTimescale (tf: Timescale) : string =
  match tf with
  | Minute -> "Minutes"
  | Hour -> "Hours"
  | Day -> "Days"
  | Week -> "Weeks"
  | Month -> "Months"
  | Year -> "Years"

let evalType (thing: Type) : string =
  match thing with
    | Kid -> "Child"
    | Plant -> "Plant"
    | Dog -> "Dog"
    | Cat -> "Cat"
    | House -> "House"
    | Fish -> "Fish"

let rec eval_fields (fields: Fields) : string =
  match fields.info with
  | [] -> "---"
  | (s1,s2)::[] -> "\t" + s1 + ": " + s2
  | (s1,s2)::fs -> 
      "\t" + s1 + ": " + s2 + """~\\""" + eval_fields {info = fs}

let eval_expr (e: Expr) =
  match e with
  | Instance (t, s, f) -> 
      """\item """ + "About " + s + ", the newest " + evalType(t) + """: ~\\"""
        + eval_fields(f) + """~\\"""
  | Location (s, ss) -> """\item """ + "Location: " + s + """~\\""" + eval_fields(ss) + """~\\"""
  | Action (s, i, t) ->
      """\item """ + "Do task " + s + " every " + string(i) + evalTimescale(t) + """~\\"""
  | _ -> failwith "Type not implemented yet, come back later"

let rec eval_list (e: Expr list) (sw_this: StreamWriter)=
  match e with
  | [] -> ""
  | e1::es -> 
      sw_this.WriteLine (eval_expr e1)
      eval_list es sw_this
      
(* Evaluates expr list *)
let eval (e: Expr list): string =

  // Create output LaTeX script
  let path = @"..\CareDoc.tex"
  use sw = File.CreateText path
  sw.WriteLine setup
  sw.WriteLine (section "Placeholder title")
  sw.WriteLine open_items

  let res = eval_list e sw

  // Close LaTex script
  sw.WriteLine close_items
  sw.WriteLine wrapup
  "File CareDoc.tex has been generated in current working directory"