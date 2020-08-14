namespace MMath.Core.Calculator

open MMath.Core
open System.Text.RegularExpressions

type CancellationToken = System.Threading.CancellationToken

type Token =
    | Start
    | End
    | Err of ErrorType * startIndex: int * endIndex: int
    | NumT of Frac * startIndex: int * endIndex: int
    | LBracket of startIndex: int
    | RBracket of startIndex: int
    | OpT of Operator * startIndex: int
    | Rtn
    | Declare of Declaration * Token list * line: int
    | Invoke of string * args: Token list * startIndex: int * endIndex: int
and Operator = Add | Sub | Mul | Div
and ErrorType =
    | UnexceptedChar
    | UnexceptedRBracket
    | UnexceptedReturn
    | NeedNum
    | BracketNotMatch
and Declaration = Var of string | Func of string * string list

module Lex =
    let private numRegex = Regex(@"\-?(\d+){1}(\.\d+)?")
    let private varRegex = Regex(@"^\s*\w+\s*=\s*\S{1}.*$")
    let private fnRegex = Regex(@"^\s*fn\s+\w+(\s+\w+)+\s*=\s*$")

    let private getErrFromState state err =
        match state with
        | NumT (_, si, ei) -> Err (err, si, ei)
        | LBracket i | RBracket i | OpT (_, i) -> Err (err, i, i)
        | _ -> Err (err, 0, 0)

    let private getToken char i =
        match char with
        | '+' -> OpT (Add, i)
        | '-' -> OpT (Sub, i)
        | '*' -> OpT (Mul, i)
        | '/' -> OpT (Div, i)
        |  _  -> OpT (Add, i)

    let private getRBrkToken brackets i =
        match brackets with
        | []        ->   [], Err (UnexceptedRBracket, i, i)
        | _ :: tail -> tail, RBracket i

    let private getNumOrElseToken input i allowMinus =
        let m = numRegex.Match (input, i)
        if m.Success && m.Index = i then
            let ni = i + m.Length - 1
            let num = m.Value |> Frac.From |> Result.unwrap
            if not allowMinus && num < Frac._0
            then        0, Err (UnexceptedChar, i + 1, i + 1)
            else (ni + 1), NumT (num, i, ni)
        else            0, Err (UnexceptedChar, i + 1, i + 1)

    let rec private lexFSM_ (input: string) i brackets (isInFunc, indent) states lastState =
        match lastState with
        | Start ->
            if i + 1 > input.Length then lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) End
            else
                match input.[i] with
                |  ' ' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) (lastState :: states) Rtn
                |  '(' ->
                    let b = LBracket (i + 1)
                    lexFSM_ input (i + 1) (b :: brackets) (isInFunc, indent) (lastState :: states) b
                | _ ->
                    match getDeclarationToken input i with
                    | Some (token, ni) -> lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
                    | None ->
                        let ni, token = getNumOrElseToken input i true
                        lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
        | End ->
            match brackets with
            | [] -> lastState :: states |> List.except [ Rtn ]
            | LBracket ni :: _ ->
                lexFSM_ input i [] (isInFunc, indent) (lastState :: states) <| Err (BracketNotMatch, ni, ni)
            | _ ->
                lexFSM_ input i [] (isInFunc, indent) (lastState :: states) <| getErrFromState states.Head BracketNotMatch
        | Err _ -> lastState :: states |> List.except [ Rtn ]
        | NumT _ ->
            if i + 1 > input.Length then lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) End
            else
                let i = i + 1
                match input.[i - 1] with
                |  ' ' -> lexFSM_ input i brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) Rtn
                |  ')' ->
                    let brks, token = getRBrkToken brackets i
                    lexFSM_ input i brks (isInFunc, indent) (lastState :: states) token
                | '+' | '-' | '*' | '/' -> lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) <| getToken input.[i - 1] i
                | _   -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, i, i)
        | LBracket _ ->
            if i + 1 > input.Length then lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (NeedNum, i, i)
            else
                match input.[i] with
                |  ' ' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) (lastState :: states) Rtn
                |  '(' ->
                    let b = LBracket (i + 1)
                    lexFSM_ input (i + 1) (b :: brackets) (isInFunc, indent) (lastState :: states) b
                | _ ->
                    let ni, token = getNumOrElseToken input i true
                    lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
        | RBracket _ ->
            if i + 1 > input.Length then lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) End
            else
                let i = i + 1
                match input.[i - 1] with
                |  ' ' -> lexFSM_ input i brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) Rtn
                |  ')' ->
                    let brks, token = getRBrkToken brackets i
                    lexFSM_ input i brks (isInFunc, indent) (lastState :: states) token
                | '+' | '-' | '*' | '/' -> lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) <| getToken input.[i - 1] i
                | _   -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, i, i)
        | OpT _ ->
            if i + 1 > input.Length then lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (NeedNum, i, i)
            else
                match input.[i] with
                |  ' ' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) (lastState :: states) Rtn
                |  '(' ->
                    let b = LBracket (i + 1)
                    lexFSM_ input (i + 1) (b :: brackets) (isInFunc, indent) (lastState :: states) b
                | _ ->
                    let ni, token = getNumOrElseToken input i false
                    lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
        | Declare _ | Rtn ->
            if i + 1 > input.Length then lexFSM_ input i brackets (isInFunc, indent) (lastState :: states) End
            else
                match input.[i] with
                |  ' ' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) states lastState
                | '\n' -> lexFSM_ input (i + 1) brackets (isInFunc, indent) (lastState :: states) Rtn
                |  '(' ->
                    match states |> List.except [ Rtn ] with
                    | NumT _ :: _ -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, i, i)
                    | _ ->
                        let b = LBracket (i + 1)
                        lexFSM_ input (i + 1) (b :: brackets) (isInFunc, indent) (lastState :: states) b
                | ')' ->
                    let brks, token = getRBrkToken brackets (i + 1)
                    lexFSM_ input (i + 1) brks (isInFunc, indent) (lastState :: states) token
                | '+' | '-' | '*' | '/' ->
                    match states |> List.except [ Rtn ] with
                    | LBracket index :: _ -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, index, index)
                    | Start :: _ -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, i, i)
                    | _ -> lexFSM_ input (i + 1) brackets (isInFunc, indent) (lastState :: states) <| getToken input.[i] (i + 1)
                | _ ->
                    match states |> List.except [ Rtn ] with
                    | NumT _ :: _ -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedReturn, i, i)
                    | RBracket index :: _ -> lexFSM_ input 0 [] (isInFunc, indent) (lastState :: states) <| Err (UnexceptedChar, index, index)
                    | OpT _ :: _ | LBracket _ :: _ ->
                        let ni, token = getNumOrElseToken input i true
                        lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
                    | _ ->
                        match getDeclarationToken input i with
                        | Some (token, ni) -> lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token
                        | None ->
                            let ni, token = getNumOrElseToken input i true
                            lexFSM_ input ni brackets (isInFunc, indent) (lastState :: states) token

    and private getDeclarationToken input i =
        let m1 = varRegex.Match(input, i)
        //let m2 = funRegex.Match(input, i)
        if m1.Success && m1.Index = i then
            let equI = m1.Value.IndexOf '='
            let name = m1.Value.[0 .. equI - 1].Trim()
            let value = lexFSM_ (m1.Value.[equI + 1..].Trim()) 0 [] (true, -1) [] Start
            Some <| (Declare (Var name, value, i), i + m1.Length)
        //elif m2.Success && m2.Index = i then
        //    printfn "%s" m2.Value
        else None

    let lexicalAnalysis str = lexFSM_ str 0 [] (false, -1) [] Start

type Tree =
    | Nil
    | Num of Frac
    | Op of Operator * left: Tree * right: Tree
    | Arg of string

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
    let rec private getTree_ expList needMul needFindBrk input (cancellationToken: CancellationToken) =
        match input with
        | Start :: End :: [] -> Nil
        | Start :: (NumT _ as n) :: End :: [] -> getNum expList n
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
                        let exp = getTree_ expList true false (Start :: input.[ y + 1 .. x - 1 ] @ [ End ]) cancellationToken
                        let input' = input |> replace y x (NumT (Frac._0, -1, expList.Length))
                        getTree_ (expList @ [ exp ]) needMul true input' cancellationToken
                    | None -> failwithf "%i处的括号不匹配" x
                | None ->
                    let symbols = input |> List.filter (function
                        |OpT (o, _) ->
                            match o with
                            | Mul | Div -> needMul
                            | _ -> not needMul
                        | _ -> false)
                    match symbols with
                    | [] -> getTree_ expList false false input cancellationToken
                    | _ ->
                        let hi, ti = find连续op的头尾i -1 -1 symbols input
                        let hi, ti = hi - 1, ti + 2
                        let exp = input |> List.take ti
                        let exp = getSingleExpression expList Nil (exp |> List.skip hi)
                        let input' = input |> replace hi (ti - 1) (NumT (Frac._0, -1, expList.Length))
                        getTree_ (expList @ [ exp ]) needMul false input' cancellationToken
            else Nil
    let rec private makeDeclarations varList tokens (cancellationToken: CancellationToken) =
        if cancellationToken.IsCancellationRequested 
        then ([], Start :: End :: [])
        else
            match tokens with
            | Start :: t -> makeDeclarations varList t cancellationToken
            | Declare (Var n, ts, _) :: t -> 
                let varList' = (n, getTree_ [] true true ts cancellationToken) :: varList
                makeDeclarations varList' t cancellationToken
            | _ -> (List.rev varList, Start :: tokens)
    let rec private checkStruct acceptDeclaration tokens (cancellationToken: CancellationToken) =
        if cancellationToken.IsCancellationRequested 
        then false
        else
            match tokens with
            | Declare _ :: t ->
                if acceptDeclaration then checkStruct acceptDeclaration t cancellationToken
                else false
            | Start :: t | End :: t | Err _ :: t | Rtn :: t -> checkStruct acceptDeclaration t cancellationToken
            | _ :: t -> checkStruct false t cancellationToken
            | [] -> true
    let getTree cancellationToken tokens = 
        if checkStruct true tokens cancellationToken then
            let valList, tokens' = makeDeclarations [] tokens cancellationToken
            (valList, getTree_ [] true true tokens' cancellationToken)
        else ([], Nil)

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

    let rec private getExpression_ state stack now (cancellationToken: CancellationToken) =
        if not cancellationToken.IsCancellationRequested then
            match now with
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