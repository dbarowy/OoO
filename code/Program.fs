open Output
open eval
open System.IO
open System
open Parser

[<EntryPoint>]
let main args = 

  match args with
  | [||] -> printfn "Usage: dotnet run <file.txt>"
  | some_args ->
    let file = args[0]
    let text = File.ReadAllText file
    match (parse text) with
    | Some ast ->
        let res = eval ast
        // let r = executeShellCommand """..\pdflatex ScheduleDoc.tex""" |> Async.RunSynchronously
        // if r.ExitCode = 0 then
        //   printfn "%s" r.StandardOutput
        // else
        //   eprintfn "%s" r.StandardError
        //   Environment.Exit(r.ExitCode)
        printfn "%A" res

    | None -> printfn "Invalid Program"
  0
