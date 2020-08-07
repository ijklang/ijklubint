namespace MMath.Core

open System
open System.Diagnostics

/// 为 UnsignedBigInterger 提供辅助工具。
[<Obsolete "此模块只应被 UnsignedBigInterger 类使用。"; DebuggerStepThrough>]
module internal ubintHlpr =
    let cachedC = "0123456789_".ToCharArray()

    let rec private getBytesFromULong_ state num =
        if num = 0UL then
            if state |> List.isEmpty then (sbyte num :: state)
            else state |> List.rev
        else getBytesFromULong_ ((num % 10UL |> sbyte) :: state) (num / 10UL)
    let getBytesFromULong = getBytesFromULong_ []

    let rec private trim0FromTail_ state =
        match state with
        | 0y :: t when not t.IsEmpty -> trim0FromTail_ t
        | _ -> state
    let trim0FromTail = List.rev >> trim0FromTail_ >> List.rev

    let rec private getBytesFromString_ state str =
        if String.IsNullOrEmpty str then
            if state |> List.isEmpty then [0y]
            else state |> trim0FromTail
        else getBytesFromString_ ((cachedC |> Array.findIndex ((=) str.[0]) |> sbyte) :: state) (str.Substring 1)
    let getBytesFromString = getBytesFromString_ []

    let rec private compare_ i (x: sbyte list) (y: sbyte list) len =
        if x.[i] > y.[i] then true
        elif x.[i] = y.[i] && i + 1 < len then compare_ (i + 1) x y len
        else false
    let compare x y = compare_ 0 (List.rev x) (List.rev y) x.Length

    let rec private isPowOf10_ i (units: sbyte list) len =
        if units.[i] = 0y && i + 1 < len then isPowOf10_ (i + 1) units len
        elif i + 1 = len then units.[i] = 1y
        else false
    let isPowOf10 units = isPowOf10_ 0 units units.Length

    let getPowOf10 = List.filter ((=) 0y) >> List.length

    let append0 n units = units @ [ for _ = 1 to n do 0y ]

    let toString units =
        if units |> List.isEmpty then "0"
        else
            units
            |> List.rev
            |> List.map (fun i -> cachedC.[int i])
            |> List.toArray
            |> String

    let rec append0ToHead n bytes =
        if n = 0 then bytes
        else append0ToHead (n - 1) (0y :: bytes)

    let rec private get要进的数 state isSub k =
        if isSub then -state
        elif k - state > 9y then get要进的数 (state + 10y) isSub k
        else state

    let rec private 进位_ i flag state (units: sbyte list) isSub len =
        let 本位 =
            if flag = 0y then units.[i]
            else units.[i] + flag
        if
            if isSub then 本位 > -1y
            else 本位 < 10y
        then
            if i + 1 = len then 本位 :: state |> List.rev
            else 进位_ (i + 1) 0y (本位 :: state) units isSub len
        else
            let num = get要进的数 10y isSub 本位
            if i + 1 = len then (num / 10y) :: (本位 - num) :: state |> List.rev
            else 进位_ (i + 1) (num / 10y) ((本位 - num) :: state) units isSub len
    let 进位 isSub units = 进位_ 0 0y [] units isSub units.Length

    let rec trim0For'n'Times n state =
        if n = 0 then state
        else trim0For'n'Times (n - 1) (state |> List.tail)

// "此构造已弃用。"
#nowarn "44"
open ubintHlpr
#warn "44"

[<CompiledName "UnsignedBigInterger"; DebuggerStepThrough>]
type Ubint private (units) =
    static let rec 逼近 x y changeState state =
        let cs = changeState state
        if x > state * y && x >= cs * y
        then 逼近 x y changeState cs
        else state

    static let rec cal_opDiv x y n e =
        if x < (n + e) * y then
            if e = Ubint._1 then n
            else cal_opDiv x y n (e / Ubint._10)
        else
            if e = Ubint._1
            then e
            else (e / Ubint._10)
            |> cal_opDiv x y (逼近 x y ((+) e) n)

    let _isPowOf10 = lazy isPowOf10 units
    member _.Units     with get() = units
    member _.IsPowOf10 with get() = if _isPowOf10.IsValueCreated then _isPowOf10.Value else _isPowOf10 |> ``|Lazy|``

    static member FromString value =
        if value |> String.forall (fun c -> cachedC |> Array.contains c) then
            (value.Replace("_", "")) |> getBytesFromString |> Ubint |> Ok
        else sprintf "“%s”不是有效的正整数，应输入一个正整数。" value |> Error

    static member _0  = Ubint [ 0y ]
    static member _1  = Ubint [ 1y ]
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
                let xlen = units.Length
                let ylen = yUnits.Length

                if xlen < ylen then -1
                elif xlen > ylen then 1
                elif compare units yUnits then 1
                else -1
            else -1

    static member op_Explicit value = value |> getBytesFromULong |> Ubint
    static member op_Explicit (value: uint8) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit (value: uint16) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit (value: uint32) = value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0y then -value |> uint64 |> Ubint.op_Explicit
        else value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0s then -value |> uint64 |> Ubint.op_Explicit
        else value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0 then -value |> uint64 |> Ubint.op_Explicit
        else value |> uint64 |> Ubint.op_Explicit
    static member op_Explicit value =
        if value < 0L then -value |> uint64 |> Ubint.op_Explicit
        else value |> uint64 |> Ubint.op_Explicit

    static member (+) (x, y) =
        if   x = Ubint._0 then y
        elif y = Ubint._0 then x
        else
            let n = x.Units.Length - y.Units.Length
            let ns1 = if n >= 0 then x.Units else append0 -n x.Units
            let ns2 = if n <= 0 then y.Units else append0 n y.Units

            (ns1, ns2)
            ||> List.map2 (+)
            |> 进位 false
            |> Ubint

    static member (-) (x, y) =
        if   x = y then Ubint._0
        elif y = Ubint._0 then x
        elif x > y then
            let n = x.Units.Length - y.Units.Length
            let ns1 = if n >= 0 then x.Units else append0 -n x.Units
            let ns2 = if n <= 0 then y.Units else append0 n y.Units

            (ns1, ns2)
            ||> List.map2 (-)
            |> 进位 true
            |> trim0FromTail
            |> Ubint
        else failwith "被减数不能小于减数。"

    static member (*) (x, y) =
        if   x = Ubint._0 || y = Ubint._0 then Ubint._0
        elif x = Ubint._1 then y
        elif y = Ubint._1 then x
        elif x.IsPowOf10 || y.IsPowOf10 then
            (if x.IsPowOf10 then y else x).Units
            |> ((if x.IsPowOf10 then x else y).Units |> getPowOf10 |> append0ToHead)
            |> Ubint
        else
            y.Units
            |> List.mapi (fun i y ->
                x.Units
                |> List.map ((*) y)
                |> 进位 false
                |> append0ToHead i
                |> Ubint
            )
            |> List.fold (+) Ubint._0
            |> (fun num -> num.Units |> trim0FromTail |> Ubint)

    static member (/) (x, y) =
        if   y > x then Ubint._0
        elif y = Ubint._0 then failwith "除数不能为零。"
        elif y = Ubint._1 then x
        elif y = x then Ubint._1
        elif y.IsPowOf10 then
            x.Units
            |> (y.Units |> getPowOf10 |> trim0For'n'Times)
            |> Ubint
        else
            let e = 逼近 x y ((*) Ubint._10) Ubint._1
            cal_opDiv x y e e

    static member (%) (x: Ubint, y) = x - y * (x / y)