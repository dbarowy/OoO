module Evaluator

open AST
open System.IO
open Template


let evalTimeframe (tf: Timescale) : string =
  match tf with
  | Minute -> "We are on the scale of minutes"
  | Hour -> "We are on the scale of hours"
  | Day -> "We are on the scale of days"
  | Week -> "We are on the scale of weeks"
  | Month -> "We are on the scale of months"
  | Year -> "We are on the scale of years"

let evalType (thing: Type) : string =
  match thing with
    | Kid -> "Child"
    | Plant -> "Plant"
    | Dog -> "Dog"
    | Cat -> "Cat"
    | House -> "House"
    | Fish -> "Fish"


let eval_one (e: Expr) (sw_this: StreamWriter)=
  match e with
  | Instance (t, s) ->
      let this_line = "Introducing " + s + ", the newest " + evalType(t) + "\n"
      sw_this.WriteLine this_line
  | _ -> printfn "Not writing to file because type has not been instituted yet"

(* Evaluates exprs with the assumption that ALL VALUES of a
certain kind of expr will be next to each other in the input .txt file *)
let eval (e: Expr): string =

  // Create output LaTeX script
  let path = @"..\CareDoc.tex"
  use sw = File.CreateText path
  sw.WriteLine setup
  sw.WriteLine (section "Placeholder title")
  sw.WriteLine open_items

  eval_one e sw

  // Close LaTex script
  sw.WriteLine close_items
  sw.WriteLine wrapup
  "File CareDoc.tex has been generated in current working directory"