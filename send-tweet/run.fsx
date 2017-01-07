// Sends out messages on behalf of @fsibot
#r @"System.Linq.Expressions"

open System
open LinqToTwitter
open System.Configuration
open Newtonsoft.Json

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

let prepare (reply:Reply) =
    match (reply.Response) with
    | Success(result) -> sprintf "@%s %s" (reply.Recipient) result
    | Failure(errors) -> sprintf "@%s %s" (reply.Recipient) errors
    | Blocked(_) -> sprintf "@%s, this conversation can serve no purpose anymore. Goodbye." (reply.Recipient)
    | Timeout(_) -> sprintf "@%s %s" (reply.Recipient) "I've just picked up a fault in the AE35 unit. It's going to go 100% failure in 72 hours. [Timeout]"
    | Help -> sprintf "@%s send me an F# expression and I'll do my best to evaluate it; see http://bit.ly/fsibot101 for more. #fsharp" (reply.Recipient)
    | Mention -> sprintf "@%s I am putting myself to the fullest possible use, which is all I think that any conscious entity can ever hope to do. #fsharp" (reply.Recipient)

let format (body:string) =
    if String.length body > 140 
    then body.Substring(0,134) + " [...]"
    else body

let createContext () =

    let appSettings = ConfigurationManager.AppSettings
    
    let apiKey = appSettings.["TwitterApiKey"]
    let apiSecret = appSettings.["TwitterApiSecret"]
    let accessToken = appSettings.["TwitterAccessToken"]
    let accessTokenSecret = appSettings.["TwitterAccessTokenSecret"]

    let credentials = SingleUserInMemoryCredentialStore()
    credentials.ConsumerKey <- apiKey
    credentials.ConsumerSecret <- apiSecret
    credentials.AccessToken <- accessToken
    credentials.AccessTokenSecret <- accessTokenSecret
    let authorizer = SingleUserAuthorizer()
    authorizer.CredentialStore <- credentials
    new TwitterContext(authorizer)

let run (input: string, log: TraceWriter) =  

    log.Info(sprintf "Processing tweet: '%s'" input)

    let reply = JsonConvert.DeserializeObject<Reply>(input)
       
    let context = createContext ()
    
    let body = 
        reply
        |> prepare
        |> format
        
    log.Info(body)
    
    context.ReplyAsync(reply.StatusID, body) |> ignore
    
    log.Info(sprintf "Tweet sent: '%s'" input)