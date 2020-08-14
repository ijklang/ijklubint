namespace MMath.Core.Calculator

open MMath.Core
open System.Text.RegularExpressions

type CancellationToken = System.Threading.CancellationToken

type Token =
    | Start
    | End
    | ErrT of ErrorType * ln: int * startIndex: int * endIndex: int
    | NumT of Frac * ln: int * startIndex: int * endIndex: int
    | LBracket of ln: int * startIndex: int
    | RBracket of ln: int * startIndex: int
    | OpT of Operator * ln: int * startIndex: int
    | Rtn of ln: int
    | Declare of Declaration * Token list * startLn: int * endLn: int
    | Invoke of name: string * ln: int * startIndex: int * endIndex: int
and Operator = Add | Sub | Mul | Div
and ErrorType =
    | UnexceptedChar
    | UnexceptedRBracket
    | UnexceptedReturn
    | NeedNum
    | BracketNotMatch
    | UnknownIdentifier
and Declaration = Var of string | Func of string * string list

module Lex =
    let private numRegex = Regex(@"\-?(\d+){1}(\.\d+)?")
    let private varRegex = Regex(@"^\s*\w+\s*=\s*\S{1}.*$")
    let private invokeRegex = Regex(@"[a-z]\w*\s+")
    let private fnRegex = Regex(@"^\s*fn\s+\w+(\s+\w+)+\s*=\s*$")

    let private getErrFromState state err =
        match state with
        | LBracket (l, i) | RBracket (l, i) | OpT (_, l, i) -> ErrT (err, l, i,  i)
        | NumT (_, l, si, ei) -> ErrT (err, l, si, ei)
        | _ -> ErrT (err, 1, 0, 0)

    let private getToken char l i =
        match char with
        | '+' -> OpT (Add, l, i)
        | '-' -> OpT (Sub, l, i)
        | '*' -> OpT (Mul, l, i)
        | '/' -> OpT (Div, l, i)
        |  _  -> OpT (Add, l, i)

    let private getRBrkToken brackets l i =
        match brackets with
        |        [] ->   [], ErrT (UnexceptedRBracket, l, i, i)
        | _ :: tail -> tail, RBracket (l, i)

    let private getNumOrElseToken (input: string) l i allowMinus =
        if not allowMinus && input.[i] = '-'
        then ErrT (UnexceptedChar, l, i + 1, i + 1), 0
        else
            let m1 = numRegex.Match (input, i)
            let m2 = invokeRegex.Match (input, i)
            if m1.Success && m1.Index = i then
                let ni = i + m1.Length - 1
                let num = m1.Value |> Frac.From |> Result.unwrap
                NumT (num, l, i, ni), (ni + 1)
            elif m2.Success && m2.Index = i then
                let name = m2.Value.Trim ()
                Invoke (name, l, m2.Index, m2.Index + name.Length - 1), m2.Index + m2.Length - 1
            else ErrT (UnexceptedChar, l, i + 1, i + 1), 0

    let rec private isLastNotRtnIsStartOrDeclareToken tokens =
        match tokens with
        | [] | Start :: _ | Declare _ :: _ -> true
        | Rtn _ :: t -> isLastNotRtnIsStartOrDeclareToken t
        | _ :: _ -> false

    let rec private lexFSM_ lineStack ln (input: string) i brackets (isInFunc, indent) states lastState =
        match lastState with
        | ErrT _ | End when brackets |> List.isEmpty ->
            let r = lastState :: states
            let e =r |> List.filter (function Rtn _ -> true | _ -> false)
            List.except e r
        | End when not <| List.isEmpty brackets ->
            lexFSM_ [] ln input 0 [] (isInFunc, indent) (lastState :: states)
            <|  match brackets with
                | LBracket (l, ni) :: _ -> ErrT (BracketNotMatch, l, ni, ni)
                | _ -> getErrFromState states.Head BracketNotMatch
        | _ ->
            if i + 1 > input.Length then
                if
                    match lastState with
                    | LBracket _ | OpT _ -> true
                    | _ -> false
                then lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (NeedNum, ln, i, i)
                else
                    match lineStack with
                    |     [] -> lexFSM_ []      ln input 0 brackets (isInFunc, indent) (lastState :: states)    End
                    | h :: t -> lexFSM_  t (ln + 1)    h 0 brackets (isInFunc, indent) (lastState :: states) <| Rtn ln
            else
                match input.[i] with
                |  ' ' -> lexFSM_ lineStack ln input (i + 1) brackets (isInFunc, indent)               states  lastState
                |  '(' ->
                    match lastState with
                    | Start | LBracket _ | OpT _ | Rtn _ when isLastNotRtnIsStartOrDeclareToken states ->
                        let b = LBracket (ln, i + 1)
                        lexFSM_ lineStack ln input (i + 1) (b :: brackets) (isInFunc, indent) (lastState :: states) b
                    | Rtn l when not <| isLastNotRtnIsStartOrDeclareToken states ->
                        lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedReturn, l, 0, 0)
                    | _ -> lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedChar, ln, i + 1, i + 1)
                |  ')' ->
                    match lastState with
                    | NumT _ | Invoke _ | RBracket _ | Invoke _ | Rtn _ when isLastNotRtnIsStartOrDeclareToken states ->
                        let brks, token = getRBrkToken brackets ln (i + 1)
                        lexFSM_ lineStack ln input (i + 1) brks (isInFunc, indent) (lastState :: states) token
                    | Rtn l when not <| isLastNotRtnIsStartOrDeclareToken states ->
                        lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedReturn, l, 0, 0)
                    | _ -> lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedChar, ln, i + 1, i + 1)
                | '+' | '-' | '*' | '/' ->
                    match lastState with
                    | NumT _ | RBracket _ | Invoke _ ->
                        lexFSM_ lineStack ln input (i + 1) brackets (isInFunc, indent) (lastState :: states) <| getToken input.[i] ln (i + 1)
                    | _ -> lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedChar, ln, i + 1, i + 1)
                | _ ->
                    match lastState with
                    | NumT _ | RBracket _ -> lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedChar, ln, i + 1, i + 1)
                    | Start | LBracket _ | OpT _ ->
                        let token, ni =
                            if lastState = Start then
                                match getDeclarationToken input ln with
                                | None   -> getNumOrElseToken input ln i true
                                | Some t -> (t, 0)
                            else getNumOrElseToken input ln i (match lastState with LBracket _ -> true | _ -> false)
                        lexFSM_ lineStack ln input ni brackets (isInFunc, indent) (lastState :: states) token
                    | Rtn _ when isLastNotRtnIsStartOrDeclareToken states ->
                        let token, ni =
                            match getDeclarationToken input ln with
                            | Some t -> (t, 0)
                            | None -> getNumOrElseToken input ln i true
                        lexFSM_ lineStack ln input ni brackets (isInFunc, indent) (lastState :: states) token
                    | Rtn l when not <| isLastNotRtnIsStartOrDeclareToken states ->
                        lexFSM_ [] ln input 0 [] (isInFunc, indent) [] <| ErrT (UnexceptedReturn, l, 0, 0)
                    | Invoke (_, _, _, ei) ->
                        lexFSM_ lineStack ln input (ei + 1) brackets (isInFunc, indent) states lastState
                    | Declare _ ->
                        match lineStack with
                        |     [] -> lexFSM_ []      ln input 0 brackets (isInFunc, indent) (lastState :: states)    End
                        | h :: t -> lexFSM_  t (ln + 1)    h 0 brackets (isInFunc, indent) (lastState :: states) <| Rtn ln
                    | _ -> 
                        lexFSM_ lineStack ln input (i + 1) brackets (isInFunc, indent) states lastState

    and private getDeclarationToken input ln =
        let m1 = varRegex.Match input
        //let m2 = funRegex.Match (input, i)
        if m1.Success then
            let equI = m1.Value.IndexOf '='
            let name = m1.Value.[0 .. equI - 1].Trim ()
            let value = lexFSM_ [] 1 (m1.Value.[equI + 1..].Trim() + " ") 0 [] (true, -1) [] Start
            Some <| Declare (Var name, value, ln, ln)
        //elif m2.Success && m2.Index = i then
        //    printfn "%s" m2.Value
        else None

    let lexicalAnalysis lnList = lexFSM_ (lnList |> List.tail) 1 lnList.Head 0 [] (false, -1) [] Start

type Tree =
    | Nil
    | Num of Frac
    | Op of Operator * left: Tree * right: Tree
    | Err of ErrorType * ln: int * startIndex: int * endIndex: int

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
        | NumT (n, _, i, j) ->
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
    let private getVar token (vars: System.Collections.Immutable.ImmutableDictionary<string, Tree>) = 
        match token with
        | Invoke (name, ln, si, ei) ->
            if vars.ContainsKey name 
            then vars.[name] 
            else Err (UnknownIdentifier, ln, si, ei)
        | _ -> Num Frac._0
    let rec private getSingleExpression expList vars state input =
        match input with
        | [] -> state
        | NumT _ as n  :: [] ->
            if state = Nil then getNum expList n
            else state
        | Invoke _ as n :: [] ->
            if state = Nil then getVar n vars
            else state

        | NumT _ as n1 :: OpT (o, _, _) :: (NumT _ as n2) :: [] ->
            if state = Nil
            then Op (o, getNum expList n1, getNum expList n2)
            else Op (o, state, getNum expList n2)
        | NumT _ as n1 :: OpT (o, _, _) :: (Invoke _ as n2) :: [] ->
            if state = Nil
            then Op (o, getNum expList n1, getVar n2 vars)
            else Op (o, state, getVar n2 vars)
        | Invoke _ as n1 :: OpT (o, _, _) :: (NumT _ as n2) :: [] ->
            if state = Nil
            then Op (o, getVar n1 vars, getNum expList n2)
            else Op (o, state, getNum expList n2)
        | Invoke _ as n1 :: OpT (o, _, _) :: (Invoke _ as n2) :: [] ->
            if state = Nil
            then Op (o, getVar n1 vars, getVar n2 vars)
            else Op (o, state, getVar n2 vars)

        | NumT _ as n1 :: OpT (o, _, _) :: (NumT _ as n2) :: _ ->
            if state = Nil 
            then getSingleExpression expList vars (Op (o, getNum expList n1, getNum expList n2)) (input.Tail.Tail)
            else getSingleExpression expList vars (Op (o, state, getNum expList n2)) (input.Tail.Tail)
        | NumT _ as n1 :: OpT (o, _, _) :: (Invoke _ as n2) :: _ ->
            if state = Nil
            then getSingleExpression expList vars (Op (o, getNum expList n1, getVar n2 vars)) (input.Tail.Tail)
            else getSingleExpression expList vars (Op (o, state, getVar n2 vars)) (input.Tail.Tail)
        | Invoke _ as n1 :: OpT (o, _, _) :: (NumT _ as n2) :: _ ->
            if state = Nil
            then getSingleExpression expList vars (Op (o, getVar n1 vars, getNum expList n2)) (input.Tail.Tail)
            else getSingleExpression expList vars (Op (o, state, getNum expList n2)) (input.Tail.Tail)
        | Invoke _ as n1 :: OpT (o, _, _) :: (Invoke _ as n2) :: _ ->
            if state = Nil 
            then getSingleExpression expList vars (Op (o, getVar n1 vars, getVar n2 vars)) (input.Tail.Tail)
            else getSingleExpression expList vars (Op (o, state, getVar n2 vars)) (input.Tail.Tail)
        | _ -> state
    let rec private getTree_ expList needMul needFindBrk input (cancellationToken: CancellationToken) vars =
        match input with
        | Start :: End :: [] -> Nil
        | Start :: (NumT _ as n) :: End :: [] -> getNum expList n
        | Start :: (Invoke _ as n) :: End :: [] -> getVar n vars
        | _ ->
            if not cancellationToken.IsCancellationRequested then
                let rbrki =
                    if needFindBrk then
                        input |> List.tryFindIndex (function RBracket _ -> true | _ -> false)
                    else None
                match rbrki with
                | Some x ->
                    let lbrki = input |> List.take x |> List.tryFindIndexBack (function LBracket _ -> true | _ -> false)
                    match lbrki with
                    | Some y ->
                        let exp = getTree_ expList true false (Start :: input.[ y + 1 .. x - 1 ] @ [ End ]) cancellationToken vars
                        let input' = input |> replace y x (NumT (Frac._0, 0, -1, expList.Length))
                        getTree_ (expList @ [ exp ]) needMul true input' cancellationToken vars
                    | None -> failwithf "%i处的括号不匹配" x
                | None ->
                    let symbols = input |> List.filter (function
                        |OpT (o, _, _) ->
                            match o with
                            | Mul | Div -> needMul
                            | _ -> not needMul
                        | _ -> false)
                    match symbols with
                    | [] -> getTree_ expList false false input cancellationToken vars
                    | _ ->
                        let hi, ti = find连续op的头尾i -1 -1 symbols input
                        let hi, ti = hi - 1, ti + 2
                        let exp = input |> List.take ti
                        let exp = getSingleExpression expList vars Nil (exp |> List.skip hi)
                        let input' = input |> replace hi (ti - 1) (NumT (Frac._0, 0, -1, expList.Length))
                        getTree_ (expList @ [ exp ]) needMul false input' cancellationToken vars
            else Nil
    let rec private makeDeclarations vars states tokens (cancellationToken: CancellationToken) =
        if cancellationToken.IsCancellationRequested
        then ([], Start :: End :: [])
        else
            match tokens with
            | Start :: t -> makeDeclarations vars states t cancellationToken
            | Declare (Var n, ts, _, _) :: t ->
                let varList' = (n, getTree_ [] true true ts cancellationToken vars) :: states
                makeDeclarations vars varList' t cancellationToken
            | _ -> (List.rev states, Start :: tokens)
    let rec private checkStruct acceptDeclaration tokens (cancellationToken: CancellationToken) =
        if cancellationToken.IsCancellationRequested
        then false
        else
            match tokens with
            | Declare _ :: t ->
                if acceptDeclaration then checkStruct acceptDeclaration t cancellationToken
                else false
            | Start :: t | End :: t | ErrT _ :: t | Rtn _ :: t -> checkStruct acceptDeclaration t cancellationToken
            | _ :: t -> checkStruct false t cancellationToken
            | [] -> true

    let getTree cancellationToken tokens vars =
        if checkStruct true tokens cancellationToken then
            let varList, tokens' = makeDeclarations vars [] tokens cancellationToken
            let mutable vars = vars
            for (name, value) in varList do
                vars <-
                    if vars.ContainsKey name
                    then (vars.Remove name).Add (name, value)
                    else vars.Add (name, value)
            (varList, getTree_ [] true true tokens' cancellationToken vars)
        else ([], Nil)

type Expression =
    | ErrE of ErrorType * ln: int * startIndex: int * endIndex: int
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

    let rec private getExpression_ state stack now (cancellationToken: CancellationToken) =
        if not cancellationToken.IsCancellationRequested then
            match now with
            | Err (error, ln, si, ei) -> ErrE (error, ln, si, ei) :: []
            | Nil ->
                match stack with
                | h :: t -> getExpression_ (NumE Frac._0 :: state |> checkList) t h cancellationToken
                | [] -> NumE Frac._0 :: state |> checkList
            | Num n ->
                match stack with
                | h :: t -> getExpression_ (NumE n :: state |> checkList) t h cancellationToken
                | [] -> NumE n :: state |> checkList
            | Op (operator, left, right) -> getExpression_ (OpE operator :: state) (right :: stack) left cancellationToken
        else [ NumE Frac._0 ]
    let toExpression cancellationToken tree = getExpression_ [] [] tree cancellationToken