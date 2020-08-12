open MMath.Core
open MMath.Core.Calculator
open System

let rec readln i stack =
    let ln =
        if i = 0 
        then printf "> "
        else printf "- "
        |> Console.ReadLine
    if ln.Contains ";;" then
        ln.[0..ln.IndexOf ';' - 1] :: stack
    else readln (i + 1) (ln :: stack)
    
[<EntryPoint>]
let main _ =
    printfn "输入要计算的表达式，并用半角双分号结束输入。"
    printfn "输入 cls;; 可以清除屏幕上的内容。"
    printfn ""
    let stopWatch = System.Diagnostics.Stopwatch()
    while true do
        try
            let str = readln 0 [] |> List.rev |> String.Concat
            if str.Trim () = "cls" then Console.Clear()
            else
                stopWatch.Reset()
                stopWatch.Start()
                let lexResult =
                    str
                    |> Lex.lexicalAnalysis
                    |> (fun l ->
                        match l.Head with
                        | Err (error, index) ->
                            Console.ForegroundColor <- ConsoleColor.Red
                            match error with
                            | UnexceptedChar       -> "不是所需的字符"
                            | UnexceptedRBracket   -> "没有与之对应的‘(’"
                            | UnexceptedWhiteSpace -> "后不应该有空格"
                            | NeedNum              -> "后应该有一个数字"
                            | BracketNotMatch      -> "没有与之对应的‘)’"
                            |> printfn "第%i列的字符%s。" index
                            Console.ForegroundColor <- ConsoleColor.White
                            []
                        | _ -> l |> List.rev
                    )
                match lexResult with
                | [] | [ Start; End ] -> stopWatch.Stop()
                | _ ->
                    let num = 
                        lexResult 
                        |> Tree.getTree 
                        |> Expression.toExpression 
                        |> List.head 
                        |> (fun i -> match i with NumE n -> n | _ -> Frac._0) 
                    stopWatch.Stop()
                    Console.ForegroundColor <- ConsoleColor.Green
                    printf "= "
                    num.ToString() |> printfn "%s"
                    Console.ForegroundColor <- ConsoleColor.Gray
                    printfn "本次计算共耗时%f秒。" stopWatch.Elapsed.TotalSeconds
                printfn ""
        with e -> 
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "%A" e
            Console.ForegroundColor <- ConsoleColor.White
    0