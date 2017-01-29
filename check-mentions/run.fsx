(*
Every 2 minutes, check Twitter for potential new mentions.
Enqueue every mention found, and update the last Tweet ID.
*)

#r @"System.Linq.Expressions"
open System
open System.Configuration
open LinqToTwitter
open Newtonsoft.Json

type Mention = {
    StatusID:uint64
    Text:string
    Author:string
    }

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

let run (
    timer:TimerInfo, 
    mentionsQueue:ICollector<string>, 
    friendsQueue:ICollector<string>, 
    previousID:string, 
    updatedID: string byref, 
    log:TraceWriter) =

    log.Info("Starting to check for new mentions")
    
    let context = createContext ()
   
    let lastID =
        if String.IsNullOrEmpty previousID
        then None
        else Some(uint64 previousID)
        
    log.Info(sprintf "Last ID: %A" lastID)

    let mentions = 
        match lastID with
        | None ->
            query { 
                for tweet in context.Status do 
                where (tweet.Type = StatusType.Mentions)
                select tweet 
                }
        | Some(id) ->
            query { 
                for tweet in context.Status do 
                where (tweet.Type = StatusType.Mentions && tweet.SinceID = id)
                where (tweet.StatusID <> id)
                select tweet 
                }
        |> Seq.toList

    // enqueue message to be processed
    mentions
    |> Seq.iter (fun status ->
    
        let mention = {
            StatusID = status.StatusID
            Text = status.Text
            Author = status.User.ScreenNameResponse
            }
            
        let msg =    
            mention
            |> JsonConvert.SerializeObject
        
        mentionsQueue.Add(msg)
            
        log.Info(msg))

    // enqueue userID to be followed    
    mentions
    |> Seq.iter (fun status ->
    
        let userName = status.User.ScreenNameResponse
        let userID = status.User.UserID       
        friendsQueue.Add(string userID)
        
        sprintf "Following %s" userName
        |> log.Info
        )

    // update the last ID processed
    let updatedLastID =
        match mentions with
        | [] -> lastID
        | hd::_ -> hd.StatusID |> Some
        
    if (updatedLastID <> lastID) 
    then 
        match updatedLastID with
        | None -> ignore ()
        | Some id -> 
            updatedID <- string id

    log.Info("Completed checking for new mentions")