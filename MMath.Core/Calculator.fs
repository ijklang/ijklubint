namespace MMath.Core.Calculator

open MMath.Core

type Token =
    | Start
    | End
    | Err of ErrorType * int
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

    let ccache = "0123456789".ToCharArray()

    let getErrFromState state err =
        match state with
        | NumT (_, _, i) | NumHolder (_, i) | LBracket i | RBracket i | Minus i | Dot i | OpT (_, i) -> Err (err, i)
        | _ -> Err (err, -1)

    let rec private lexFSM_ input i brackets numCaches acceptDot (states: Token list) lastState =
        match numCaches, lastState with
        | _ :: _, Err _ -> lexFSM_ input 0 [] [] true states lastState
        | _, NumHolder _ | _, Dot _ | _, Minus _ | [], _ ->
            match lastState with
            | Start ->
                match input with
                | [] -> lexFSM_ input i brackets numCaches true (lastState :: states) End
                | head :: tail ->
                    let i = i + 1
                    match head with
                    | ' ' -> lexFSM_ tail i brackets numCaches true states lastState
                    | '(' ->
                        let b = LBracket i
                        lexFSM_ tail i (b :: brackets) numCaches true (lastState :: states) b
                    | '-' ->
                        let m = Minus i
                        lexFSM_ tail i brackets (m :: numCaches) true (lastState :: states) m
                    | _ when ccache |> Array.contains head ->
                        let c = NumHolder (head, i)
                        lexFSM_ tail i brackets (c :: numCaches) true (lastState :: states) c
                    | _ ->
                        lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedChar, i)
            | End ->
                match brackets with
                | [] -> lastState :: states
                | LBracket ni :: _ ->
                    lexFSM_ input i [] [] true (lastState :: states) <| Err (BracketNotMatch, ni)
                | _ ->
                    lexFSM_ input i [] [] true (lastState :: states) <| getErrFromState states.Head BracketNotMatch
            | Err _ -> lastState :: states
            | NumHolder (_, index) ->
                match input with
                | [] -> lexFSM_ input i brackets numCaches true (lastState :: states) End
                | head :: tail ->
                    let i = i + 1
                    match head with
                    | ' ' -> lexFSM_ tail 0 [] [] acceptDot states <| Err (UnexceptedWhiteSpace, index)
                    | ')' ->
                        match brackets with
                        | [] ->
                            lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedRBracket, i)
                        | _ :: tail_ ->
                            lexFSM_ tail i tail_ numCaches true (lastState :: states) <| RBracket i
                    | '.' when acceptDot ->
                        if i - index = 1 then
                            let d = Dot i
                            lexFSM_ tail i brackets (d :: numCaches) false (lastState :: states) d
                        else lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedWhiteSpace, index)
                    | '+' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Add, i)
                    | '-' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Sub, i)
                    | '*' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Mul, i)
                    | '/' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Div, i)
                    | _ when ccache |> Array.contains head ->
                        if i - index = 1 then
                            let n = NumHolder (head, i)
                            lexFSM_ tail i brackets (n :: numCaches) acceptDot (lastState :: states) <| n
                        else lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedWhiteSpace, index)
                    | _   -> lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedChar, i)
            | LBracket _ ->
                match input with
                | [] -> lexFSM_ input 0 [] [] true (lastState :: states) <| Err (NeedNum, i)
                | head :: tail ->
                    let i = i + 1
                    match head with
                    | ' ' -> lexFSM_ tail i brackets numCaches true states lastState
                    | '(' ->
                        let b = LBracket i
                        lexFSM_ tail i (b :: brackets) numCaches true (lastState :: states) b
                    | '-' ->
                        let m = Minus i
                        lexFSM_ tail i brackets (m :: numCaches) true (lastState :: states) m
                    | _ when ccache |> Array.contains head ->
                        let n = NumHolder (head, i)
                        lexFSM_ tail i brackets (n :: numCaches) true (lastState :: states) n
                    | _ -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| Err (UnexceptedChar, i)
            | RBracket _ ->
                match input with
                | [] -> lexFSM_ input i brackets numCaches true (lastState :: states) End
                | head :: tail ->
                    let i = i + 1
                    match head with
                    | ' ' -> lexFSM_ tail i brackets numCaches true states lastState
                    | ')' ->
                        match brackets with
                        | [] ->
                            lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedRBracket, i)
                        | _ :: tail_ ->
                            lexFSM_ tail i tail_ numCaches true (lastState :: states) <| RBracket i
                    | '+' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Add, i)
                    | '-' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Sub, i)
                    | '*' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Mul, i)
                    | '/' -> lexFSM_ tail i brackets numCaches true (lastState :: states) <| OpT (Div, i)
                    | _   -> lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedChar, i)
            | Dot index ->
                match input with
                | [] -> lexFSM_ input 0 [] [] true (lastState :: states) <| Err (NeedNum, i)
                | head :: tail ->
                    let i = i + 1
                    match head with
                    | ' ' -> lexFSM_ tail 0 [] [] true states <| Err (UnexceptedWhiteSpace, index)
                    | _ when ccache |> Array.contains head ->
                        let n = NumHolder (head, i)
                        lexFSM_ tail i brackets (n :: numCaches) true (lastState :: states) n
                    | _ -> lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedChar, i)
            | Minus _ | OpT _ | NumT _ ->
                match input with
                | [] -> lexFSM_ input 0 [] [] true (lastState :: states) <| Err (NeedNum, i)
                | head :: tail ->
                    match head with
                    | ' ' -> lexFSM_ tail i brackets numCaches true states lastState
                    | '(' ->
                        let b = LBracket i
                        lexFSM_ tail i (b :: brackets) numCaches true (lastState :: states) b
                    | _ when ccache |> Array.contains head ->
                        let n = NumHolder (head, i)
                        lexFSM_ tail i brackets (n :: numCaches) true (lastState :: states) n
                    | _ -> lexFSM_ tail 0 [] [] true (lastState :: states) <| Err (UnexceptedChar, i)
        | _::_, _ -> lexFSM_ input i brackets [] acceptDot ((getNumFromHolderCaches numCaches) :: (removeHolder states)) lastState
    let lexicalAnalysis (str: string) = lexFSM_ (str.ToCharArray() |> Array.toList) 0 [] [] true [] Start

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
            else     replace_ (head :: state) (i + 1) startIndex endIndex replace replaced tail
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
    let rec private getTree_ expList needMul needFindBrk input =
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
                    let exp = getTree_ expList true false (Start :: input.[ y + 1 .. x - 1 ] @ [ End ])
                    let input' = input |> replace y x (NumT (Frac._0, -1, expList.Length))
                    getTree_ (expList @ [ exp ]) needMul true input'
                | None -> failwithf "%i处的括号不匹配" x
            | None ->
                let symbols = input |> List.filter (function
                    |OpT (o, _) ->
                        match o with
                        | Mul | Div -> needMul
                        | _ -> not needMul
                    | _ -> false)
                match symbols with
                | [] -> getTree_ expList false false input
                | _ ->
                    let hi, ti = find连续op的头尾i -1 -1 symbols input
                    let hi, ti = hi - 1, ti + 2
                    let exp = input |> List.take ti
                    let exp = getSingleExpression expList Nil (exp |> List.skip hi)
                    let input' = input |> replace hi (ti - 1) (NumT (Frac._0, -1, expList.Length))
                    getTree_ (expList @ [ exp ]) needMul false input'
    let getTree tokens = getTree_ [] true true tokens

type Expression =
    | NumE of Frac
    | OpE of Operator

module Expression =
    let rec private checkList input =
        match input with
        | NumE n2 :: NumE n1 :: OpE o :: tail ->
            let num =
                match o with
                | Add -> (+) n1 n2
                | Sub -> (-) n1 n2
                | Mul -> (*) n1 n2
                | Div -> (/) n1 n2
            checkList <| NumE num :: tail
        | _ -> input

    let rec private getExpression_ state stack now =
        match now with
        | Nil ->
            match stack with
            | h :: t -> getExpression_ (NumE Frac._0 :: state |> checkList) t h
            | [] -> NumE Frac._0 :: state |> checkList
        | Num n ->
            match stack with
            | h :: t -> getExpression_ (NumE n :: state |> checkList) t h
            | [] -> NumE n :: state |> checkList
        | Op (operator, left, right) -> getExpression_ (OpE operator :: state) (right :: stack) left
    let toExpression tree = getExpression_ [] [] tree
