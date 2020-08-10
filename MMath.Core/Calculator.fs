namespace MMath.Core.Calculator

open MMath.Core

type Token =
    | Start
    | End
    | Err of ErrorType * int * char
    | NumT of Frac * int * int
    | NumHolder of char * int
    | LBracket of int
    | RBracket of int
    | Minus of int
    | Dot of int
    | OpT of Operator * int
and Operator = Add | Sub | Mul | Div
and ErrorType =
    | UnexceptedChar
    | UnexceptedRBracket
    | UnexceptedWhiteSpace
    | NeedNum
    | BracketNotMatch

module Lex =
    let rec private getNumFromHolderCaches_ (startIndex, endIndex, state) input =
        match input with
        | Dot   index             :: tail -> getNumFromHolderCaches_ (index, endIndex, '.' :: state) tail
        | Minus index             :: tail -> getNumFromHolderCaches_ (index, endIndex, '-' :: state) tail
        | NumHolder (char, index) :: tail -> getNumFromHolderCaches_ (index, (if endIndex = -1 then index else endIndex), char :: state) tail
        | _                       :: tail -> getNumFromHolderCaches_ (startIndex, endIndex, state) tail
        | [] ->
            let num = state |> List.toArray |> System.String |> Frac.From |> Result.unwrap
            NumT (num, startIndex, endIndex)
    let private getNumFromHolderCaches = getNumFromHolderCaches_ (0, -1, [])

    let rec private removeHolder input =
        match input with
        | NumHolder _ :: tail | Dot _ :: tail | Minus _ :: tail -> removeHolder tail
        | _ -> input

    let rec private lexicalAnalysisFSM_ (states: Token list) (input: string) i brackets numCache acceptDot lastReaded =
        match numCache, lastReaded with
        | _ :: _, Err _ -> lexicalAnalysisFSM_ states input i brackets [] acceptDot lastReaded
        | _, NumHolder _ | _, Dot _ | _, Minus _ | [], _ ->
            let invoke = lexicalAnalysisFSM_ (lastReaded :: states) input (i + 1)
            match lastReaded with
            | Start ->
                if i + 1 > input.Length then invoke brackets numCache true End
                elif input.[i] = ' ' then lexicalAnalysisFSM_ states input (i + 1) brackets numCache acceptDot lastReaded
                else
                    let mutable brk = brackets
                    let mutable cache = numCache
                    if input.[i] = '(' then
                        let b = LBracket i
                        brk <- b :: brk
                        b
                    elif input.[i] = '-' then
                        let n = Minus i
                        cache <- n :: cache
                        n
                    elif "0123456789".Contains input.[i..i] then
                        let n = NumHolder (input.[i], i)
                        cache <- n :: cache
                        n
                    else Err (UnexceptedChar, i, input.[i])
                    |> invoke brk cache true
            | End ->
                match brackets with
                | []               -> lastReaded :: states
                | LBracket ni :: _ -> invoke brackets numCache true (Err (BracketNotMatch, ni, input.[ni]))
                | _                -> invoke brackets numCache true (Err (BracketNotMatch, i - 2, input.[i - 2]))
            | Err _ -> lastReaded :: states
            | NumHolder (char, index) ->
                if i + 1 > input.Length then invoke brackets numCache true End
                elif input.[i] = ' ' then lexicalAnalysisFSM_ states input (i + 1) brackets numCache acceptDot lastReaded
                else
                    let mutable brk = brackets
                    let mutable cache = numCache
                    if input.[i] = ')' then
                        if brk.Length < 1 then
                            true, Err (UnexceptedRBracket, i, input.[i])
                        else
                            brk <- brk.Tail
                            true, RBracket i
                    elif acceptDot && input.[i] = '.' then
                        if i - index = 1 then
                            let d = Dot i
                            cache <- d :: cache
                            false, d
                        else false, Err (UnexceptedWhiteSpace, index, char)
                    elif input.[i] = '+' then true, OpT (Add, i)
                    elif input.[i] = '-' then true, OpT (Sub, i)
                    elif input.[i] = '*' then true, OpT (Mul, i)
                    elif input.[i] = '/' then true, OpT (Div, i)
                    elif "0123456789".Contains input.[i..i] then
                        if i - index = 1 then
                            let n = NumHolder (input.[i], i)
                            cache <- n :: cache
                            acceptDot, NumHolder (input.[i], i)
                        else acceptDot, Err (UnexceptedWhiteSpace, index, char)
                    else     acceptDot, Err (UnexceptedChar, i, input.[i])
                    ||> invoke brk cache
            | LBracket _ ->
                if i + 1 > input.Length then invoke brackets numCache true <| Err (NeedNum, i - 1, input.[i - 1])
                elif input.[i] = ' ' then lexicalAnalysisFSM_ states input (i + 1) brackets numCache acceptDot lastReaded
                else
                    let mutable brk = brackets
                    let mutable cache = numCache
                    if input.[i] = '(' then
                        let b = LBracket i
                        brk <- b :: brk
                        b
                    elif input.[i] = '-' then
                        let n = Minus i
                        cache <- n :: cache
                        n
                    elif "0123456789".Contains input.[i..i] then
                        let n = NumHolder (input.[i], i)
                        cache <- n :: cache
                        n
                    else Err (UnexceptedChar, i, input.[i])
                    |> invoke brk cache true
            | RBracket _ ->
                if i + 1 > input.Length then invoke brackets numCache true End
                elif input.[i] = ' ' then lexicalAnalysisFSM_ states input (i + 1) brackets numCache acceptDot lastReaded
                else
                    let mutable brk = brackets
                    if input.[i] = ')' then
                        if brk.Length < 1 then Err (UnexceptedRBracket, i, input.[i])
                        else
                            brk <- brk.Tail
                            RBracket i
                    elif input.[i] = '+' then OpT (Add, i)
                    elif input.[i] = '-' then OpT (Sub, i)
                    elif input.[i] = '*' then OpT (Mul, i)
                    elif input.[i] = '/' then OpT (Div, i)
                    else Err (UnexceptedChar, i, input.[i])
                    |> invoke brk numCache true
            | Dot index ->
                if i + 1 > input.Length then invoke brackets numCache true <| Err (NeedNum, i - 1, input.[i - 1])
                elif input.[i] = ' ' then invoke brackets numCache acceptDot <| Err (UnexceptedWhiteSpace, index, '.')
                else
                    let mutable cache = numCache
                    if "0123456789".Contains input.[i..i] then
                        let n = NumHolder (input.[i], i)
                        cache <- n :: cache
                        n
                    else Err (UnexceptedChar, i, input.[i])
                    |> invoke brackets cache false
            | Minus _ | OpT _ | NumT _ ->
                if i + 1 > input.Length then invoke brackets numCache true <| Err (NeedNum, i - 1, input.[i - 1])
                elif input.[i] = ' ' then lexicalAnalysisFSM_ states input (i + 1) brackets numCache acceptDot lastReaded
                else
                    let mutable brk = brackets
                    let mutable cache = numCache
                    if input.[i] = '(' then
                        let b = LBracket i
                        brk <- b :: brk
                        b
                    elif "0123456789".Contains input.[i..i] then
                        let n = NumHolder (input.[i], i)
                        cache <- n :: cache
                        n
                    else Err (UnexceptedChar, i, input.[i])
                    |> invoke brk cache true
        | _::_, _ -> lexicalAnalysisFSM_ ((getNumFromHolderCaches numCache) :: (removeHolder states)) input i brackets [] acceptDot lastReaded
    let lexicalAnalysis str = lexicalAnalysisFSM_ [] str 0 [] [] true Start

type Tree =
    | Nil
    | Num of Frac
    | Op of Operator * left: Tree * right: Tree

module Tree =
    let rec private replace_ state i startIndex endIndex replace replaced input =
        match input with
        | [] -> state
        | head :: tail ->
            if startIndex <= i && i <= endIndex then
                if replaced then
                     replace_ state              (i + 1) startIndex endIndex replace replaced tail
                else replace_ (replace :: state) (i + 1) startIndex endIndex replace true tail
            else replace_ (head :: state) (i + 1) startIndex endIndex replace replaced tail
    let private replace startIndex endIndex replace = replace_ [] 0 startIndex endIndex replace false >> List.rev
    let private getNum (expList: Tree list) num =
        match num with
        | NumT (n, i, j) ->
            if i = -1 then expList.[j]
            else Num n
        | _ -> Nil
    let rec private find连续op的头尾i startIndex endIndex filterResult inputList =
        match filterResult with
        | [] -> startIndex, endIndex
        | head :: tail ->
            let endIndex' = inputList |> List.findIndex ((=) head)
            if endIndex = -1 || endIndex' - endIndex = 2 then
                let startIndex' = if startIndex = -1 then endIndex' else startIndex
                find连续op的头尾i startIndex' endIndex' tail inputList
            else startIndex, endIndex
    let rec private getSingleExpression expList state input =
        match input with
        | [] -> state
        | NumT _ as n  :: [] ->
            if state = Nil then getNum expList n
            else state
        | NumT _ as n1 :: OpT (o, _) :: (NumT _ as n2) :: [] ->
            if state = Nil 
            then Op (o, getNum expList n1, getNum expList n2)
            else Op (o, state, getNum expList n2)
        | NumT _ as n1 :: OpT (o, _) :: (NumT _ as n2) :: _ ->
            if state = Nil then getSingleExpression expList (Op (o, getNum expList n1, getNum expList n2)) (input.Tail.Tail)
            else getSingleExpression expList (Op (o, state, getNum expList n2)) (input.Tail.Tail)
        | _ -> state
    let rec private getExpression_ expList needMul needFindBrk input =
        match input with
        | Start :: End :: [] -> Nil
        | Start :: (NumT _ as n) :: End :: [] -> getNum expList n
        | _ ->
            let rbrki = 
                if needFindBrk then 
                    input |> List.tryFindIndex (function RBracket _ -> true | _ -> false)
                else None
            match rbrki with
            | Some x ->
                let lbrki = input |> List.take x |> List.tryFindIndexBack (function LBracket _ -> true | _ -> false)
                match lbrki with
                | Some y ->
                    let exp = getExpression_ expList true false (Start :: input.[ y + 1 .. x ] @ [ End ])
                    let input' = input |> replace y x (NumT (Frac._0, -1, expList.Length))
                    getExpression_ (expList @ [ exp ]) needMul true input'
                | None -> failwithf "%i处的括号不匹配" x
            | None ->
                let symbols = input |> List.filter (function
                    |OpT (o, _) ->
                        match o with
                        | Mul | Div -> needMul
                        | _ -> not needMul
                    | _ -> false)
                match symbols with
                | [] -> getExpression_ expList false false input
                | _ ->
                    let hi, ti = find连续op的头尾i -1 -1 symbols input
                    let hi, ti = hi - 1, ti + 2
                    let exp = input |> List.take ti
                    let exp' = getSingleExpression expList Nil (exp |> List.skip hi)
                    let input' = input |> replace hi (ti - 1) (NumT (Frac._0, -1, expList.Length))
                    getExpression_ (expList @ [ exp' ]) needMul false input'
    let getTree = getExpression_ [] true true

type Expression =
    | NumE of Frac
    | OpE of Operator

module Expression =
    let rec private checkOutList input =
        match input with
        | NumE n2 :: NumE n1 :: OpE o :: tail -> 
            let num =
                match o with
                | Add -> (+) n1 n2
                | Sub -> (-) n1 n2
                | Mul -> (*) n1 n2
                | Div -> (/) n1 n2
            checkOutList <| NumE num :: tail
        | _ -> input

    let rec private getExpression_ state stack now =
        match now with
        | Nil -> 
            match stack with
            | h :: t -> getExpression_ (NumE Frac._0 :: state |> checkOutList) t h
            | [] -> NumE Frac._0 :: state |> checkOutList
        | Num n -> 
            match stack with
            | h :: t -> getExpression_ (NumE n :: state |> checkOutList) t h
            | [] -> NumE n :: state |> checkOutList
        | Op (operator, left, right) -> getExpression_ (OpE operator :: state) (right :: stack) left
    let toExpression = getExpression_ [] []

