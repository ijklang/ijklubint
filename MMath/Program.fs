open MMath.Core.Calculator
open System
open System.Collections.Immutable

let rec readln i stack =
    let ln =
        if i = 0
        then printf "> "
        else printf "- "
        |> Console.ReadLine
    if isNull ln then None
    elif ln.Contains ";;" then
        Some <| ln.[0..ln.IndexOf ';' - 1] :: stack
    else readln (i + 1) (ln :: stack)

let showErr (str: string) error si ei ln sln name =
    Console.ForegroundColor <- ConsoleColor.Red
    let i = if si - 20 < 0 then 0 else si - 20
    let j = if si + 20 > str.Length then str.Length - 1 else si + 19
    let k = si - i - 1
    let k = if k < 0 then 0 else k
    let str = str.[i .. j]
    let str = 
        if String.IsNullOrEmpty name 
        then str
        else 
            let eqi = str.IndexOf '='
            str.[eqi + 1..].Trim()
    printfn ""
    if String.IsNullOrEmpty name then
        printfn "在输入的第%i行[%i, %i]范围内发现问题：" ln si ei
    else
        printfn "在第%i行（“%s”声明的第%i行）[%i, %i]范围内发现问题：" sln name ln si ei
    printfn "%s" str
    printfn "%s%s" <| String.init k (fun _ -> " ") <| String.init (ei - si + 1) (fun _ -> "~")
    match error with
    | UnexceptedChar     -> "该字符不是所需的字符"
    | UnexceptedRBracket -> "没有与之对应的‘(’"
    | UnexceptedReturn   -> "回车前不是声明语句"
    | NeedNum            -> "该字符后应该有一个数字"
    | BracketNotMatch    -> "没有与之对应的‘)’"
    | UnknownIdentifier  -> "没有在当前上下文中找到具有该名称的变量"
    |> (printfn "%s%s" <| String.init k (fun _ -> " "))
    Console.ForegroundColor <- ConsoleColor.Gray

let varList = System.Collections.Generic.Dictionary<string, Tree> ()

let cli (stopwatch: System.Diagnostics.Stopwatch) (cancellationToken: System.Threading.CancellationToken) =
    async {
        printfn ""
        match readln 0 [] with
        | None -> ()
        | Some rlnList ->
            let rlnList = rlnList |> List.rev
            let str = rlnList |> String.concat "\n"
            if str.Trim () = "cls" then Console.Clear ()
            elif str.Trim () = "exit" then Environment.Exit 0
            elif str.Trim () = "vars" then
                printfn ""
                printfn "已声明 %i 个变量：" varList.Count
                for i in varList do
                    Console.ForegroundColor <- ConsoleColor.Cyan
                    printf "%s" i.Key
                    Console.ForegroundColor <- ConsoleColor.Gray
                    printf " = "
                    Console.ForegroundColor <- ConsoleColor.Green
                    printfn "%A" i.Value
                Console.ForegroundColor <- ConsoleColor.Gray
            else
                stopwatch.Reset ()
                stopwatch.Start ()
                let lexResult =
                    rlnList
                    |> Lex.lexicalAnalysis
                    |> (fun l ->
                        match l.Head with
                        | ErrT (error, ln, si, ei) -> showErr (rlnList.[ln - 1]) error si ei ln 0 String.Empty; []
                        | _ ->
                            if
                                l
                                |> List.filter(function
                                    | Declare (id, h :: _, sln, _) ->
                                        match h with
                                        | ErrT (error, ln, si, ei) -> showErr (rlnList.[ln - 1]) error si ei (sln + ln - 1) sln (match id with Var n -> n | Func (n, _) -> n); true
                                        | _ -> false
                                    | _ -> false
                                )
                                |> List.isEmpty
                            then
                                l
                                |> List.map(function
                                    | Declare (id, t, sln, eln) -> Declare (id, List.rev t, sln, eln)
                                    | i -> i
                                )
                                |> List.rev
                            else []
                    )
                match lexResult with
                | [] | [ Start; End ] -> stopwatch.Stop()
                | _ ->
                    let newVar, tree =
                        (lexResult, varList.ToImmutableDictionary())
                        ||> Tree.getTree cancellationToken
                    for (name, value) in newVar do
                        match value with
                        | Err (error, ln, si, ei) -> showErr (rlnList.[ln - 1]) error si ei ln 0 String.Empty
                        | _ ->
                            if varList.ContainsKey name then
                                varList.[name] <- value
                            else varList.Add (name, value)
                    let num =
                        tree
                        |> Expression.toExpression cancellationToken
                        |> List.head
                        |> (function 
                            | NumE n -> Some n 
                            | ErrE (error, ln, si, ei) -> showErr (rlnList.[ln - 1]) error si ei ln 0 String.Empty; None
                            | _ -> Some MMath.Core.Frac._0)
                    stopwatch.Stop ()
                    match num with
                    | Some num ->
                        if not cancellationToken.IsCancellationRequested then
                            Console.ForegroundColor <- ConsoleColor.Green
                            printf "= "
                            num |> MMath.Core.Frac.ToString |> printfn "%s"
                            printfn "%A" tree
                            Console.ForegroundColor <- ConsoleColor.Gray
                            printfn "本次计算共耗时%f秒。" stopwatch.Elapsed.TotalSeconds
                    | _ -> ()
    }

let mutable source: System.Threading.CancellationTokenSource = null
[<EntryPoint>]
let main _ =
    Console.CancelKeyPress.Add (fun args ->
        args.Cancel <- true
        if not <| isNull source then
            source.Cancel ()
            printfn ""
            printfn "- 中断"
    )
    printfn "输入要计算的表达式，输入完后在结尾加上半角双分号开始计算。"
    printfn "按下 Ctrl + C 可中止计算。"
    printfn "输入 cls;; 可以清除屏幕上的内容。"
    printfn "输入 vars;; 可以查看所有已声明的变量的值。"
    printfn "输入 exit;; 退出。"
    let stopwatch = System.Diagnostics.Stopwatch ()
    while true do
        try
            source <- new System.Threading.CancellationTokenSource ()
            Async.RunSynchronously (cli stopwatch source.Token, -1, source.Token)
        with e ->
            if e.GetType () <> typeof<System.OperationCanceledException> then
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "%A" e
                Console.ForegroundColor <- ConsoleColor.White
    0