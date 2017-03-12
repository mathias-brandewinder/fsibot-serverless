// attempt to run the message received as code in FSI

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Net
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Interactive.Shell
open Newtonsoft.Json

// #load "security.fsx"
// open Security

type Mention = {
    StatusID:uint64
    Text:string
    Author:string
    }

[<Measure>]type ms

type Response =
    | Success of string
    | Failure of string
    | Blocked of string
    | Timeout of int<ms>
    | Help
    | Mention

type Reply = {
    StatusID:uint64
    Recipient:string
    Response:Response 
    }

let createSession () =

    let sbOut = new Text.StringBuilder()
    let sbErr = new Text.StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    Console.SetOut(outStream)

    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let session = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream) 
    
    session, outStream

let removeFirstLines (output:string) =
    output.Split '\n'
    |> Seq.skip 6
    |> String.concat "\n"

let removeCaret (output:string) =
    output.Substring 2

let removeDoubleSemis (text:string) =
    if text.EndsWith ";;" 
    then text.Substring (0, text.Length - 2)
    else text

let simplifyExpression (text:string) =
    if text.StartsWith "val it : "
    then 
        let codeStart = text.IndexOf " = "
        text.Remove(0,codeStart + 3)
    else text

let trim (text:string) = text.Trim()
    
let cleanFsiOutput (output:string) =
    output 
    |> removeFirstLines
    |> removeCaret
    |> simplifyExpression

let runner (session:FsiEvaluationSession,output:StringWriter) (code:string) = 

    let res = session.EvalInteractionNonThrowing(code)
    
    match res with
    | _, errors when errors |> Seq.exists (fun e -> e.Severity = FSharpErrorSeverity.Error) ->
        let errorMessages =
            errors
            |> Seq.map (fun err -> err.Message)
            |> String.concat "\n"
        Failure errorMessages

    | Choice2Of2 exn, _ ->
        sprintf "%A: %s" (exn.GetType()) exn.Message
        |> Failure

    | Choice1Of2 (), warnings ->
        Success(output |> string |> cleanFsiOutput)

let fsi (timeout:int<ms>) (code:string) =  

        let source = new CancellationTokenSource()
        let token = source.Token
        let session,output = createSession ()

        let work = Task.Factory.StartNew<Response>(fun _ -> 
            runner (session,output) code)

        if work.Wait(int timeout)
        then work.Result
        else  
            source.Cancel ()
            session.Interrupt ()
            Timeout(timeout) 
(*                
    match (unsafeQuick code) with
    | Some(banned) -> Blocked(banned)
    | None ->
        let source = new CancellationTokenSource()
        let token = source.Token
        let session,output = createSession ()

        match (unsafeDeep session code) with
        | Some(banned) -> Blocked(banned)
        | None ->
            let work = Task.Factory.StartNew<Response>(fun _ -> 
                runner (session,output) code)

            if work.Wait(int timeout)
            then work.Result
            else  
                source.Cancel ()
                session.Interrupt ()
                Timeout(timeout) 
*)

let (|HelpRequest|_|) (body:string) =
    if (body.Contains("#help"))
    then Some()
    else None

let (|CodeRequest|_|) (body:string) =
    let body = 
        body 
        |> trim 
        |> removeDoubleSemis 
        |> WebUtility.HtmlDecode
    if body.StartsWith("@fsibot",StringComparison.OrdinalIgnoreCase)
    then Some(body.Substring(7))
    else None

let respond (mention:Mention) = 
    let response = 
        match (mention.Text) with
        | HelpRequest() -> Help
        | CodeRequest(code) -> fsi 15000<ms> code
        | _ -> Mention
    {
        StatusID = mention.StatusID
        Recipient = mention.Author
        Response = response
    }
    
let run (
    input: string, 
    responseQueue:string byref, 
    log: TraceWriter) = 

    log.Info(sprintf "Processing: '%s'" input)
    
    let mention = JsonConvert.DeserializeObject<Mention>(input)
    
    let response = respond mention |> JsonConvert.SerializeObject
    
    log.Info(sprintf "Response: '%A'" response)        
    
    responseQueue <- response
    
    log.Info(sprintf "Processed: '%s'" input)