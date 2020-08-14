open MMath.Core.Calculator
open System

let rec readln i stack =
    let ln =
        if i = 0
        then printf "> "
        else printf "- "
        |> Console.ReadLine
    if isNull ln then []
    elif ln.Contains ";;" then
        ln.[0..ln.IndexOf ';' - 1] :: stack
    else readln (i + 1) (ln :: stack)

let showErr (str: string) error si ei ln name =
    Console.ForegroundColor <- ConsoleColor.Red
    let i = if si - 20 < 0 then 0 else si - 20
    let j = if si + 20 > str.Length then str.Length - 1 else si + 19
    let str = str.[i .. j]
    let ni = str.IndexOf '\n'
    let str = if ni > -1 then str.[ni + 1..] else str
    let k = si - i - 1
    printfn ""
    if String.IsNullOrEmpty name then
        printfn "在输入的第%i行[%i, %i]范围内发现问题：" ln si ei
    else
        printfn "在“%s”声明的第%i行[%i, %i]范围内发现问题：" name ln si ei
    printfn "%s" str
    printfn "%s%s" <| String.init k (fun _ -> " ") <| String.init (ei - si + 1) (fun _ -> "~")
    match error with
    | UnexceptedChar     -> "该字符不是所需的字符"
    | UnexceptedRBracket -> "没有与之对应的‘(’"
    | UnexceptedReturn   -> "数字后不能有回车"
    | NeedNum            -> "该字符后应该有一个数字"
    | BracketNotMatch    -> "没有与之对应的‘)’"
    |> (printfn "%s%s" <| String.init k (fun _ -> " "))
    Console.ForegroundColor <- ConsoleColor.White

let valList = System.Collections.Generic.Dictionary<string, Tree>()

let cli (stopwatch: System.Diagnostics.Stopwatch) (cancellationToken: System.Threading.CancellationToken) =
    async{
        printfn ""
        let str = readln 0 [] |> List.rev |> String.concat "\n"
        if str.Trim () = "cls" then Console.Clear ()
        elif str.Trim () = "exit" then Environment.Exit 0
        else
            stopwatch.Reset ()
            stopwatch.Start ()
            let lexResult =
                str
                |> Lex.lexicalAnalysis
                |> (fun l ->
                    match l.Head with
                    | Err (error, si, ei) -> showErr str error si ei 0 String.Empty; []
                    | _ -> 
                        if  
                            l
                            |> List.filter(function
                                | Declare (id, h :: _, ln) -> 
                                    match h with
                                    | Err (error, si, ei) -> showErr str error si ei ln (match id with Var n -> n | Func (n, _) -> n); true
                                    | _ -> false
                                | _ -> false
                            )
                            |> List.isEmpty
                        then 
                            l 
                            |> List.map(function
                                | Declare (id, t, ln) -> Declare (id, List.rev t, ln)
                                | i -> i
                            )
                            |> List.rev
                        else []
                )
            match lexResult with
            | [] | [ Start; End ] -> stopwatch.Stop()
            | _ ->
                let tree =
                    lexResult
                    |> Tree.getTree cancellationToken
                //let num =
                //    tree
                //    |> Expression.toExpression cancellationToken
                //    |> List.head
                //    |> (fun i -> match i with NumE n -> n | _ -> Frac._0)
                stopwatch.Stop ()
                if not cancellationToken.IsCancellationRequested then
                    Console.ForegroundColor <- ConsoleColor.Green
                    printf "= "
                    //num |> Frac.ToString |> printfn "%s"
                    printfn "%A" tree
                    Console.ForegroundColor <- ConsoleColor.Gray
                    printfn "本次计算共耗时%f秒。" stopwatch.Elapsed.TotalSeconds
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