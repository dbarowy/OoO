open Evaluator
open System.IO
open Parser


// CRAZY THOUGHT: COULD I ALSO GENERATE A TEMPLATE DOCUMENT THAT OUTLINES
// USER INPUT? TO MAKE IT SUPER EASY TO USE?

[<EntryPoint>]
let main args = 
  
  // Makes a file in current directory for a user to program in
  // TO COMPLETE LATER IF TIME
//   let path = @"..\new_Schedule.txt"
//   use sw = File.CreateText path
//   sw.WriteLine "Welcome!"


  match args with
  | [||] -> printfn "Usage: dotnet run <file.txt>"
  | some_args ->
    let file = args[0]
    let text = File.ReadAllText file
    match (parse text) with
    | Some ast ->
        printfn "%A" ast
        let res = eval ast
        printfn "%A" res
    | None -> printfn "Invalid Program"
  0
