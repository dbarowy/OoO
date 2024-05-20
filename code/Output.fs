#!/usr/bin/env -S dotnet fsi

// Code taken from alexandru nenedelcu's Execute shell commands in F# blog post, Dec. 6, 2020 (https://alexn.org/blog/2020/12/06/execute-shell-command-in-fsharp/)

module Output

open System
open System.Diagnostics
open System.Threading.Tasks


type CommandResult =
  { ExitCode: int
    StandardOutput: string
    StandardError: string }

let executeCommand executable args =
  async {
    let! ct = Async.CancellationToken

    let startInfo = ProcessStartInfo()
    startInfo.FileName <- executable
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    for a in args do
      startInfo.ArgumentList.Add(a)

    use p = new Process()
    p.StartInfo <- startInfo
    p.Start() |> ignore

    let outTask =
      Task.WhenAll([|
        p.StandardOutput.ReadToEndAsync(ct);
        p.StandardError.ReadToEndAsync(ct) |])

    do! p.WaitForExitAsync(ct) |> Async.AwaitTask
    let! out = outTask |> Async.AwaitTask

    return
      { ExitCode = p.ExitCode
        StandardOutput = out.[0]
        StandardError = out.[1] }
  }

let executeShellCommand command =
  executeCommand "/usr/bin/env" [ "-S"; "bash"; "-c"; command ]

// // Invocation sample
// let r = executeShellCommand """pdflatex ScheduleDoc.tex""" |> Async.RunSynchronously

// if r.ExitCode = 0 then
//   printfn "%s" r.StandardOutput
// else
//   eprintfn "%s" r.StandardError
//   Environment.Exit(r.ExitCode)