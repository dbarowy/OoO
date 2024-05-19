module Template
open System


let open_items = """\begin{itemize}""" + "\n"
let close_items = """\end{itemize}""" + "\n"

(* Helper function to convert a number to a month *)
let getMonth (date:DateTime) =
  let mo = date.Month
  match mo with
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> "Internal error: Somehow we are getting a month number that's not 1-12"

(* Helper function to create a caretaker and date-specific title *)
let caretaker (name: string) (startDay: DateTime) (endDay: DateTime) =
  """\title{Schedule for """ + name + " from " + 
    string(startDay.DayOfWeek) + ", "  + getMonth(startDay) + " " + string(startDay.Day) + ", " + string(startDay.Year) + " to " +
    string(endDay.DayOfWeek) + ", "  + getMonth(endDay) + " " + string(endDay.Day) + ", " + string(endDay.Year) +
    """}""" + "\n"

(* Sets up the LaTeX document *)
let setup (name: string) (sDay: DateTime) (eDay: DateTime)= 
  """\documentclass{article}"""+ "\n" +
    """\usepackage{graphicx}"""+ "\n" +
    """\usepackage{times,graphicx,epstopdf,fancyhdr,amsfonts,amsthm,amsmath,algorithm,algorithmic,xspace,hyperref}"""+ "\n" +
    """\usepackage[left=1in,top=1in,right=1in,bottom=1in]{geometry}"""+ "\n" +
    """\usepackage{sect sty}"""+ "\n" +
    """\usepackage{enumerate}"""+ "\n" +
    """\usepackage{epsfig}"""+ "\n" +
    """\usepackage[space]{grffile}"""+ "\n" +
    """\usepackage{booktabs}"""+ "\n" +
    """\usepackage{forest}"""+ "\n" +
    """\usepackage{color}"""+ "\n" +
    """\usepackage{enumitem,amssymb}""" + "\n" +
    """\newlist{todolist}{itemize}{2}""" + "\n" +
    """\setlist[todolist]{label=$\square$}""" + "\n\n" +

    (caretaker name sDay eDay) +
    """\date{Made \today}"""+ "\n" +

    """\begin{document}"""+ "\n" +

    """\maketitle"""+ "\n"

(* Ends the LaTeX document *)
let wrapup =   "\n" + """\end{document}"""