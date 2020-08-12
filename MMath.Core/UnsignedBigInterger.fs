namespace MMath.Core

open System
open System.Diagnostics

/// 为 UnsignedBigInterger 的运行提供辅助工具。
[<Obsolete "此模块只应被 UnsignedBigInterger 类使用。"; DebuggerStepThrough>]
module internal ubintHlpr =
    let [<Literal>] cachedC = "0123456789_"

    let rec private getBytesFromULong_ state num =
        if num = 0UL then
            match state with
            | [] -> [ 0y ]
            | _ -> state |> List.rev
        else getBytesFromULong_ ((num % 10UL |> sbyte) :: state) (num / 10UL)
    /// 获取包含 uint64 数字 num 每一位数的 uint8 list。需要注意的是，它返回的结果是倒序的。如：123UL -> [ 3y; 2y; 1y ]。
    let getBytesFromULong num = getBytesFromULong_ [] num

    let rec private trim0FromTail_ state =
        match state with
        | 0y :: [] -> state
        | 0y :: t -> trim0FromTail_ t
        | _ -> state
    /// 去除一个用 uint8 list 倒序保存的数字的头部（list 尾）的 0 。
    let trim0FromTail num = num |> List.rev |> trim0FromTail_ |> List.rev

    let rec private getBytesFromString_ state str =
        if String.IsNullOrEmpty str then
            match state with
            | [] -> [ 0y ]
            | _ -> state |> trim0FromTail
        else getBytesFromString_ ((cachedC.IndexOf str.[0] |> sbyte) :: state) (str.[1..])
    /// 获取包含一个正整数的 string (str) 每一位数的 uint8 list。需要注意的是，它返回的结果是倒序的。如："123" -> [ 3y; 2y; 1y ]。
    let getBytesFromString str = getBytesFromString_ [] str

    let rec private compare_ i (x: sbyte list) (y: sbyte list) len =
        if x.[i] > y.[i] then true
        elif x.[i] = y.[i] && i + 1 < len then compare_ (i + 1) x y len
        else false
    /// 比较两个用 uint8 list 倒序保存的数字的大小。
    let compare x y = compare_ 0 (List.rev x) (List.rev y) x.Length

    let rec private isPowOf10_ i (units: sbyte list) len =
        if units.[i] = 0y && i + 1 < len then isPowOf10_ (i + 1) units len
        elif i + 1 = len then units.[i] = 1y
        else false
    /// 判断一个用 uint8 list 倒序保存的数字是否是 10 的幂。
    let isPowOf10 units = isPowOf10_ 0 units units.Length

    /// 计算一个用 uint8 list 倒序保存的数字是 10 的几次幂。
    let getPowOf10 n = n |> List.filter ((=) 0y) |> List.length |> uint32

    /// 在一个用 uint8 list 倒序保存的数字的头部（list 尾）补上 n 个 0 。
    let append0 n units = units @ [ for _ = 1 to n do 0y ]

    /// 将一个用 uint8 list 倒序保存的数字保存为 string 。
    let toString units =
        match units with
        | [] -> "0"
        | _ ->
            units
            |> List.rev
            |> List.map (fun i -> cachedC.[int i])
            |> List.toArray
            |> String

    /// 在一个用 uint8 list 倒序保存的数字的尾部（list 头）补上 n 个 0 。
    let rec append0ToHead n bytes =
        match n with
        | 0u -> bytes
        | _ -> append0ToHead (n - 1u) (0y :: bytes)

    let rec private get要进的数 state isSub k =
        if isSub then -state
        elif k - state > 9y then get要进的数 (state + 10y) isSub k
        else state

    let rec private 进位_ flag state isSub (units: sbyte list) =
        match units with
        | head :: tail ->
            let currentNum = if flag = 0y then head else head + flag
            if
                if isSub
                then currentNum > -1y
                else currentNum < 10y
            then 进位_ 0y (currentNum :: state) isSub tail
            else
                let num = get要进的数 10y isSub currentNum
                进位_ (num / 10y) ((currentNum - num) :: state) isSub tail
        | [] ->
            let state = if flag = 0y then state else flag :: state
            state |> List.rev
    let 进位 isSub units = 进位_ 0y [] isSub units

    let rec trim0For'n'Times n state =
        if n = 0u then state
        else trim0For'n'Times (n - 1u) (state |> List.tail)

    let rec private getNumOf0FromHead_ i input =
        match input with
        | 0y :: t -> getNumOf0FromHead_ (i + 1u) t
        | _ -> i
    let getNumOf0FromHead = getNumOf0FromHead_ 0u

// "此构造已弃用。"
#nowarn "44"
open ubintHlpr
#warn "44"

[<CompiledName "UnsignedBigInterger"; DebuggerStepThrough>]
type Ubint private (units) =
    static let rec 逼近 x y changeState state =
        let nextState = changeState state
        if x >= nextState * y
        then 逼近 x y changeState nextState
        else state

    static let rec cal_opDiv x y n e =
        if x < (n + e) * y then
            if e = Ubint._1
            then n
            else cal_opDiv x y n (e / Ubint._10)
        else
            cal_opDiv x y (逼近 x y ((+) e) n)
            <|  if e = Ubint._1
                then e
                else (e / Ubint._10)

    let _isPowOf10 = lazy isPowOf10 units
    member _.Units     with get() = units
    member _.IsPowOf10 with get() = if _isPowOf10.IsValueCreated then _isPowOf10.Value else _isPowOf10 |> ``|Lazy|``

    static member FromString value =
        if value |> String.forall (fun c -> cachedC.Contains (sprintf "%c" c)) then
            (value.Replace ("_", String.Empty)) |> getBytesFromString |> Ubint |> Ok
        else sprintf "“%s”不是有效的正整数，应输入一个正整数。" value |> Error

    [<CompiledName "Zero">]
    static member _0  = Ubint [ 0y ]
    [<CompiledName "One">]
    static member _1  = Ubint [ 1y ]
    [<CompiledName "Ten">]
    static member _10 = Ubint [ 0y; 1y ]

    static member ToString (x: Ubint) = toString x.Units
    override _.ToString () = toString units

    override _.Equals obj =
        if isNull obj then false
        elif obj :? Ubint then (obj :?> Ubint).Units = units
        else false

    interface IComparable with
        member x.CompareTo obj =
            if isNull obj then -1
            elif x.Equals obj then 0
            elif obj :? Ubint then
                let yUnits = (obj :?> Ubint).Units
                let len = units.Length - yUnits.Length
                if len = 0 then
                    if compare units yUnits then 1
                    else -1
                else len
            else -1

    static member op_Explicit value = value |> getBytesFromULong |> Ubint
    static member op_Explicit (value: uint8) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit (value: uint16) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit (value: uint32) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0y
        then -value |> uint64 |> Ubint.op_Explicit
        else  value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0s
        then -value |> uint64 |> Ubint.op_Explicit
        else  value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0
        then -value |> uint64 |> Ubint.op_Explicit
        else  value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0L
        then -value |> uint64 |> Ubint.op_Explicit
        else  value |> uint64 |> Ubint.op_Explicit

    static member (+) (x, y) =
        if   x = Ubint._0 then y
        elif y = Ubint._0 then x
        else
            let n = x.Units.Length - y.Units.Length

            if   n = 0 then x.Units, y.Units
            elif n > 0
            then            x.Units, append0 n y.Units
            else append0 -n x.Units, y.Units
            ||> List.map2 (+)
            |> 进位 false
            |> Ubint

    static member (-) (x, y) =
        if   x = y then Ubint._0
        elif y = Ubint._0 then x
        elif x > y then
            let n = x.Units.Length - y.Units.Length

            if   n = 0 then x.Units, y.Units
            elif n > 0
            then            x.Units, append0 n y.Units
            else append0 -n x.Units, y.Units
            ||> List.map2 (-)
            |> 进位 true
            |> trim0FromTail
            |> Ubint
        else failwith "被减数不能小于减数。"

    static member MulByPowOf10 (x: Ubint, p) = append0ToHead p x.Units |> Ubint
    member x.MulByPowOf10 p = Ubint.MulByPowOf10 (x, p)

    static member (*) (x, y) =
        if   x = Ubint._0 || y = Ubint._0 then Ubint._0
        elif x = Ubint._1 then y
        elif y = Ubint._1 then x
        elif x.IsPowOf10 || y.IsPowOf10 then
            let t1, t2 = if x.IsPowOf10 then y, x else x, y
            t1.MulByPowOf10 (getPowOf10 t2.Units)
        else
            y.Units
            |> List.mapi (fun i y ->
                x.Units
                |> List.map ((*) y)
                |> 进位 false
                |> append0ToHead (uint32 i)
                |> Ubint
            )
            |> List.fold (+) Ubint._0
            |> (fun num -> num.Units |> trim0FromTail |> Ubint)

    static member DivByPowOf10 (x: Ubint, p) = trim0For'n'Times p x.Units |> Ubint
    member x.DivByPowOf10 p = Ubint.DivByPowOf10 (x, p)

    static member (/) (x, y) =
        if   y > x then Ubint._0
        elif y = Ubint._0 then failwith "除数不能为零。"
        elif y = Ubint._1 then x
        elif y = x then Ubint._1
        elif y.IsPowOf10 then x.DivByPowOf10 (y.Units |> getPowOf10)
        else
            let nx0 = getNumOf0FromHead x.Units
            let ny0 = getNumOf0FromHead y.Units
            if nx0 <> 0u && ny0 <> 0u then
                let n = if nx0 > ny0 then ny0 else nx0
                (/) (x.DivByPowOf10 n) (y.DivByPowOf10 n)
            else
                let e = 逼近 x y ((*) Ubint._10) Ubint._1
                cal_opDiv x y e e

    static member (%) (x: Ubint, y) = x - y * (x / y)