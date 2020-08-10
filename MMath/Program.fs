open System
open MMath.Core
open MMath.Core.Calculator

[<EntryPoint>]
let main _ =
    let stopWatch = System.Diagnostics.Stopwatch()
    while true do
        try
            let str =
                printf "> "
                |> Console.ReadLine
            stopWatch.Reset()
            stopWatch.Start()
            let lexResult =
                str
                |> Lex.lexicalAnalysis
                |> (fun l ->
                    match l.Head with
                    | Err (error, index, char) ->
                        Console.ForegroundColor <- ConsoleColor.Red
                        match error with
                        | UnexceptedChar       -> "不是所需的字符"
                        | UnexceptedRBracket   -> "没有与之对应的‘(’"
                        | UnexceptedWhiteSpace -> "后不应该有空格"
                        | NeedNum              -> "之后应该有一个数字"
                        | BracketNotMatch      -> "没有与之对应的‘)’"
                        |> printfn "第%i列的‘%c’%s。" (index + 1) char
                        Console.ForegroundColor <- ConsoleColor.White
                        []
                    | _ -> l |> List.rev
                )
            let tree =
                if lexResult.IsEmpty then Nil
                else Tree.getTree lexResult
            let num = tree |> Expression.toExpression |> List.head |> (fun i -> match i with NumE n -> n | _ -> Frac._0) 
            stopWatch.Stop()
            Console.ForegroundColor <- ConsoleColor.Green
            printf "<=> "
            tree |> printfn "%A"
            Console.ForegroundColor <- ConsoleColor.White
            printf "  = "
            num.ToString() |> printfn "%s"
            Console.ForegroundColor <- ConsoleColor.Gray
            printfn "本次计算共耗时%f秒。" stopWatch.Elapsed.TotalSeconds
            printfn ""
            Console.ForegroundColor <- ConsoleColor.White
        with e -> 
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "%A" e
            Console.ForegroundColor <- ConsoleColor.White
    0