open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Net
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices

module TypeUsage =

//    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    module Extraction =
        let listGuarded f =
            try
                f ()
            with
            | _ -> []

        /// Extract entities from an entity (It can be an abreviation for a type)
        let rec extractFromEntity (e: FSharpEntity): FSharpEntity list =
            if e.IsFSharpAbbreviation then
                extractFromType e.AbbreviatedType
            else
                [e]

        /// Extract entities from a type "string list option" would give String, list<'a> and option<'a>
        and extractFromType (t: FSharpType): FSharpEntity list =
            if t.IsAbbreviation then
                extractFromType t.AbbreviatedType
            else if t.IsFunctionType then
                extractFromTypeGenericArguments t
            else
                (if t.HasTypeDefinition then extractFromEntity t.TypeDefinition else [])
                    @ (extractFromTypeGenericArguments t)

        /// Extract entities from type arguments
        /// (Tuple members & function type parameters are considered generic arguments)
        and extractFromTypeGenericArguments (t: FSharpType): FSharpEntity list =
            listGuarded (fun () -> t.GenericArguments |> List.ofSeq ) |> List.collect extractFromType

        /// Get entities for the types containing the member
        ///  (Return both where they are implemented and what type they extends for extension methods)
        let getEnclosingEntities (f: FSharpMemberOrFunctionOrValue) =
            listGuarded (fun () -> [f.EnclosingEntity])
            @
            listGuarded (fun () -> [f.LogicalEnclosingEntity])

        let rec extractFromExpr (expr: FSharpExpr) =
            // Guard against: FSharp.Compiler.Service cannot yet return this kind of pattern match
            let entitiesFromExpr = listGuarded (fun () ->
                match expr with
                | BasicPatterns.Call (_,f,_,_,_) -> getEnclosingEntities f
                | BasicPatterns.Value f -> getEnclosingEntities f
                | BasicPatterns.Lambda (f,_) -> getEnclosingEntities f
                | BasicPatterns.NewObject (f,_,_) -> getEnclosingEntities f
                | BasicPatterns.LetRec (f,_) -> f |> List.collect (fun (f, _) -> getEnclosingEntities f)
                | BasicPatterns.Let ((f,_),_) -> getEnclosingEntities f
                | BasicPatterns.ValueSet (f,_) -> getEnclosingEntities f
                | BasicPatterns.TryWith (_,f,_,f2,_) -> (getEnclosingEntities f) @ (getEnclosingEntities f2)
                | _ -> [])

            // Guard against: FSharp.Compiler.Service cannot yet return this kind of pattern match
            let subExpr = listGuarded (fun () -> expr.ImmediateSubExpressions)
            extractFromType expr.Type
                @ (entitiesFromExpr |> List.collect extractFromEntity)
                @ (subExpr |> List.collect extractFromExpr)

        let rec extractFromDecl decl =
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
                subDecls |> List.collect extractFromDecl
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
                extractFromExpr e
            | FSharpImplementationFileDeclaration.InitAction(e) ->
                extractFromExpr e

        let extractFromFile (f :FSharpImplementationFileContents) =
            f.Declarations |> List.collect extractFromDecl

    let writeAllTextAsync path (content: string) = async {
        use s = File.OpenWrite(path)
        use w = new StreamWriter(s)
        do! w.WriteAsync(content) |> Async.AwaitTask
    }

    let parseAndCheckCode input = async {
        let filePath = System.IO.Path.GetTempFileName()
        File.Delete(filePath)

        let filePath = Path.ChangeExtension(filePath, "fsx")
        do! writeAllTextAsync filePath input
        
        let checker = FSharpChecker.Create(keepAssemblyContents=true)

        let! projOptions = checker.GetProjectOptionsFromScript(filePath, input)
        let! result = checker.ParseAndCheckProject(projOptions)

        File.Delete(filePath)

        return result
    }

    let extractTypesFromCode input = async {
        let! checkProjectResults = parseAndCheckCode input
        let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles |> List.tryHead
        match checkedFile with
        | Some checkedFile -> return Extraction.extractFromFile checkedFile |> List.distinct
        | None -> return []
    }

    let extractTypesFullNamesFromCode input = async {
        let! types = extractTypesFromCode input

        return types |> List.choose (fun t -> t.TryFullName)
    }

module Checker =

    open TypeUsage

    let blacklist = 
        [ 
            "System.IO"
            "System.Net"
            "System.Threading"
            "System.Reflection"
            "System.Diagnostics"
            "System.Environment"
            "System.AppDomain"
            "System.Runtime"
            "Console"
            "Microsoft"
            "System.Configuration"
        ]

    let whitelist = [
        "System.IO.TextWriter"
        "Microsoft.FSharp"
        ]

    type Blocked = | Blocked of string

    let unsafeQuick (code:string) =
        blacklist 
        |> Seq.tryFind (fun banned -> 
            code.Contains banned)

    let blacklisted (typeName:string) =
        blacklist
        |> Seq.exists (typeName.StartsWith)

    let whitelisted (typeName:string) =
        whitelist
        |> Seq.exists (typeName.StartsWith)

    let unsafeDeep (code:string) =

        let types = 
            TypeUsage.extractTypesFullNamesFromCode code 
            |> Async.RunSynchronously 
            |> List.sort
                 
        types
        |> List.filter (fun name -> not (whitelisted name))
        |> List.filter (blacklisted)
        |> function
           | [] -> None
           | hd::_  -> Some(hd)
