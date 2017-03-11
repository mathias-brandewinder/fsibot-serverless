(*
Sanity check: does the message contain obvious
security problems?
*)

open System
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Interactive.Shell

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

type Blocked = | Blocked of string

let unsafeQuick (code:string) =
    blacklist 
    |> Seq.tryFind (fun banned -> 
        code.Contains banned)

// deep check

let explode (name:string) =
    name.Split '.' |> Set.ofArray

let checkLongIdent (longIdent:LongIdent) =
    let strings = 
        longIdent 
        |> List.map string 
        |> Set.ofList
    blacklist 
    |> List.tryFind (fun banned -> 
        Set.isSubset (explode banned) strings)
    |> Option.map Blocked

let forbidden = function
    | LongIdentWithDots(longIdent,_) -> 
        checkLongIdent longIdent

let rec check (exp:SynExpr) = 
    match exp with
    | SynExpr.AddressOf(_,x,_,_) -> check x
    | SynExpr.App(_,_,x1,x2,_) -> 
        match check x1 with
        | None -> check x2
        | Some(x) -> Some(x)
    | SynExpr.ArbitraryAfterError(_) -> None 
    | SynExpr.ArrayOrList(_,xs,_) -> 
        let issues = xs |> List.choose (check)
        match (issues.IsEmpty) with
        | true -> None
        | false -> issues.Head |> Some
    | SynExpr.ArrayOrListOfSeqExpr(_,x,_) -> check x
    | SynExpr.Assert(x,_) -> check x
    | SynExpr.CompExpr(_,_,x,_) -> check x
    | SynExpr.Const(_,_) -> None
    | SynExpr.DiscardAfterMissingQualificationAfterDot(x,_) -> check x
    | SynExpr.Do(x,_) -> check x
    | SynExpr.DoBang(x,_) -> check x
    | SynExpr.DotGet(x,_,dotId,_) -> 
        match (check x) with
        | None -> forbidden dotId
        | Some(problem) -> Some(problem)
    | SynExpr.DotIndexedGet(x,_,_,_) -> check x
    | SynExpr.DotNamedIndexedPropertySet(x1,dotId,x2,x3,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> 
            match (check x2) with
            | Some(p) -> Some(p)
            | None ->
                match (check x3) with
                | Some(p) -> Some(p)
                | None -> 
                    forbidden dotId
    | SynExpr.DotIndexedSet(x1,_,x2,_,_,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.DotSet(x1,dotId,x2,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> 
            match (check x2) with
            | Some(p) -> Some(p)
             | None -> 
                forbidden dotId
    | SynExpr.Downcast(x,_,_) -> check x
    | SynExpr.Fixed(x,_) -> check x
    | SynExpr.For(_,_,x1,_,x2,x3,_) ->
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> 
            match (check x2) with
            | Some(p) -> Some(p)
            | None -> check x3
    | SynExpr.ForEach(_,_,_,_,x1,x2,_) ->
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.FromParseError(x,_) -> check x
    | SynExpr.Ident(_) -> None
    | SynExpr.IfThenElse(x1,x2,x3,_,_,_,_) ->
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> 
            match (check x2) with
            | Some(p) -> Some(p)
            | None -> 
                match x3 with
                | None -> None
                | Some(x3) -> check x3
    | SynExpr.ImplicitZero(_) -> None
    | SynExpr.InferredDowncast(x,_) -> check x
    | SynExpr.InferredUpcast(x,_) -> check x
    | SynExpr.JoinIn(x1,_,x2,_) ->
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2    
    | SynExpr.Lambda(_,_,_,x,_) -> check x
    | SynExpr.Lazy(x,_) -> check x
    | SynExpr.LetOrUse(_,_,_,x,_) -> check x
    | SynExpr.LetOrUseBang(_,_,_,_,x1,x2,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.LibraryOnlyILAssembly(_) -> Some(Blocked "Library only")
    | SynExpr.LibraryOnlyStaticOptimization(_) -> Some(Blocked "Library only")
    | SynExpr.LibraryOnlyUnionCaseFieldGet(_) -> Some(Blocked "Library only")
    | SynExpr.LibraryOnlyUnionCaseFieldSet(_) -> Some(Blocked "Library only")
    | SynExpr.LongIdent(_,dotId,_,_) -> forbidden dotId
    | SynExpr.LongIdentSet(dotId,x,_) -> 
        match (check x) with
        | Some(p) -> Some(p)
        | None -> forbidden dotId
    | SynExpr.Match(_,x,_,_,_) -> check x
    | SynExpr.MatchLambda(_) -> None
    | SynExpr.NamedIndexedPropertySet(dotId,x1,x2,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None ->
            match (check x2) with
            | Some(p) -> Some(p)
            | None -> forbidden dotId
    | SynExpr.New(_,_,x,_) -> check x
    | SynExpr.Null(_) -> None
    | SynExpr.ObjExpr(_,o,_,_,_,_) -> 
        match o with
        | Some(x,_) -> check x
        | None -> None
    | SynExpr.Paren(x,_,_,_) -> check x
    | SynExpr.Quote(x1,_,x2,_,_) ->
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.Record(baseInfo,copyInfo,fields,_) -> 
        match baseInfo with
        | None -> None
        | Some(_,x1,_,_,_) -> 
            match (check x1) with
            | Some(p) -> Some(p)
            | None ->
                match copyInfo with
                | None -> None
                | Some(x2,_) -> check x2
    | SynExpr.Sequential(_,_,x1,x2,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.StructTuple(xs,_,_) -> 
        let problems = xs |> List.choose check
        match (problems.IsEmpty) with
        | true -> None
        | false -> problems.Head |> Some
    | SynExpr.TraitCall(_,_,x,_) -> check x
    | SynExpr.TryFinally(x1,x2,_,_,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.TryWith(x,_,_,_,_,_,_) -> check x
    | SynExpr.Tuple(xs,_,_) ->
        let problems = xs |> List.choose check
        match (problems.IsEmpty) with
        | true -> None
        | false -> problems.Head |> Some    
    | SynExpr.TypeApp(x,_,_,_,_,_,_) -> check x
    | SynExpr.Typed(x,_,_) -> check x
    | SynExpr.TypeTest(x,_,_) -> check x
    | SynExpr.Upcast(x,_,_) -> check x
    | SynExpr.While(_,x1,x2,_) -> 
        match (check x1) with
        | Some(p) -> Some(p)
        | None -> check x2
    | SynExpr.YieldOrReturn(_,x,_) -> check x
    | SynExpr.YieldOrReturnFrom(_,x,_) -> check x

let safetyCheck (fsiSession:FsiEvaluationSession) (code:string) =
    let (pfr,_,_) = 
        code 
        |> fsiSession.ParseAndCheckInteraction 
        |> Async.RunSynchronously    
    let tree = pfr.ParseTree.Value
    match tree with
    | ParsedInput.ImplFile(implementation) ->
        [
            let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implementation
            for m in modules do
                let (SynModuleOrNamespace(lid, isRec, isMod, decls, _, attrs, _, _)) = m
                for decl in decls ->
                    match decl with
                    | SynModuleDecl.Attributes(_) -> None
                    | SynModuleDecl.DoExpr(_,exp,_) -> check exp
                    | SynModuleDecl.Exception(_) -> None
                    | SynModuleDecl.HashDirective(_) -> None
                    | SynModuleDecl.Let(_,bindings,_) -> 
                        let bs = 
                            bindings
                            |> List.map (fun b -> match b with | SynBinding.Binding(_,_,_,_,_,_,_,_,_,x,_,_) -> check x)
                            |> List.choose id
                        match (bs.IsEmpty) with
                        | true -> None
                        | false -> Some(bs.Head)
                        //None // CHECK
                    | SynModuleDecl.ModuleAbbrev(_,dotId,_) -> None //forbidden dotId
                    | SynModuleDecl.NamespaceFragment(a) -> 
                        match a with | SynModuleOrNamespace(a,b,c,d,e,f,g,h) -> checkLongIdent a
                    | SynModuleDecl.NestedModule(_) -> None
                    | SynModuleDecl.Open(dotId,_) -> forbidden dotId
                    | SynModuleDecl.Types(_) -> None
        ]
    | _ -> [Some(Blocked "unsupported")]

let unsafeDeep (session:FsiEvaluationSession) (code:string) = 
    let problems = 
        safetyCheck session code
        |> List.choose id
    match (problems.IsEmpty) with
    | true -> None
    | false -> 
        problems.Head 
        |> function | Blocked(name) -> Some(name)