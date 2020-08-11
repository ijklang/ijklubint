namespace MMath.Core

open System
open System.Diagnostics
open System.Text.RegularExpressions
open System.Runtime.InteropServices

/// 为 Fraction 提供辅助工具。
[<Obsolete "此模块只应被 Fraction 类使用。"; DebuggerStepThrough>]
module internal fracHlpr =
    let rec private gdc bigger smaller =
        let r = bigger % smaller
        if r = Ubint._0 then smaller
        else gdc smaller r

    let reduce numerator denominator =
        if numerator = denominator then Ubint._1, Ubint._1
        elif numerator = Ubint._0 then Ubint._0, Ubint._1
        elif numerator = Ubint._1 || denominator = Ubint._1 then numerator, denominator
        else
            let r =
                if numerator > denominator then gdc numerator denominator
                else gdc denominator numerator
            numerator / r, denominator / r
    let ubintFromInt64 = abs >> uint64 >> Ubint.op_Explicit

[<Struct>]
type FractionType =
    | Zero
    | Positive
    | Negative
    static member (~-) x =
        match x with
        | Positive -> Negative
        | Negative -> Positive
        | Zero -> Zero
    static member (=+&-) (x, y) = if x = Positive && y = Negative then Positive else Negative
    static member (=-&+) (x, y) = if x = Negative && y = Positive then Negative else Positive
    static member (=&) (x, y) = if x = y then Positive else Negative

/// 为 Fraction 提供辅助工具。
[<Obsolete "此模块只应被 Fraction 类使用。"; DebuggerStepThrough>]
module fracStrParser =
    let zero   = Regex(@"^(\-)?0+(\.0+)?$")
    let Int    = Regex(@"^\-?\d+?$")
    let Float  = Regex(@"^\-?\d+?(\.\d+)?$")
    let FloatE = Regex(@"^\-?\d(\.\d+)?E\d+?$")

    let rec ubintFromString input =
        if zero.IsMatch input then Ok (Zero, Ubint._0, Ubint._1)
        elif Int.IsMatch input then
            let isNeg = input.[0] = '-'
            Ok ((if isNeg then Negative else Positive),
                Ubint.FromString input.[(if isNeg then 1 else 0)..] |> Result.unwrap,
                Ubint._1)
        elif Float.IsMatch input then
            let str = input.Replace (".", "")
            let i = input.Length - 1 - input.IndexOf '.'
            let str' = String.init i (fun _ -> "0")
            let isNeg = input.[0] = '-'
            Ok ((if isNeg then Negative else Positive),
                Ubint.FromString str.[(if isNeg then 1 else 0)..] |> Result.unwrap,
                Ubint.FromString ("1" + str') |> Result.unwrap)
        elif FloatE.IsMatch input then
            let pow = input.[input.IndexOf '+' + 1..] |> int
            let isPowNeg = pow < 0
            let pow = String.init (abs pow) (fun _ -> "0")
            let pow = "1" + pow |> Ubint.FromString |> Result.unwrap
            let baseN = input.[0..input.IndexOf 'E' - 1] |> ubintFromString
            match baseN with
            | Ok (t, n, d) ->
                let n, d =
                    if isPowNeg
                    then n, d * pow
                    else n * pow, d
                Ok (t, n, d)
            | Error e -> Error e
        else Error <| sprintf "无法识别字符串“%s”。" input
    let ubintFromFloat (n: float) = n.ToString() |> ubintFromString |> Result.unwrap
    let ubintFromDecimal (n: decimal) = n.ToString() |> ubintFromString |> Result.unwrap

// "此构造已弃用。"
#nowarn "44"
open fracHlpr
open fracStrParser
#warn "44"

[<CompiledName "Fraction";>]
type Frac private (_type, _numerator, _denominator) =
    static let ctor (t, n, d) =
        let n, d= reduce n d
        Frac (t, n, d)

    member _.Type        with get() = _type
    member _.Numerator   with get() = _numerator
    member _.Denominator with get() = _denominator

    static member _0  = Frac (Zero, Ubint._0, Ubint._1)
    static member _1  = Frac (Positive, Ubint._1, Ubint._1)
    static member _m1 = Frac (Negative, Ubint._1, Ubint._1)

    static member From (fracType, numerator, denominator) =
        if denominator = Ubint._0 then Error "分母不能为0。"
        elif (fracType = Zero && numerator <> Ubint._0) || (numerator = Ubint._0 && fracType <> Zero) then
            Error <| sprintf "type “%A”与分数值不匹配" fracType
        else (fracType, numerator, denominator) |> ctor |> Ok
    static member From (fracType, numerator) = Frac.From (fracType, numerator, Ubint._1)
    static member From str =
        match ubintFromString str with
        | Ok (t, n, d) -> Ok <| ctor (t, n, d)
        | Error e -> Error e

    static member GetReciprocal (x: Frac) =
        match x.Type with
        | Zero -> "0没有倒数。" |> Error
        | _ -> (x.Type, x.Denominator, x.Numerator) |> Frac |> Ok

    /// n: 要保留的位数
    [<CompiledName("ToDouble")>]
    static member ToFloat (x: Frac, [<Optional; DefaultParameterValue(5u)>] decimals) =
        match x.Type with
        | Zero -> 0.
        | _ ->
            let decimals = decimals + 1u
            let u = x.Numerator.MulPowOf10 decimals / x.Denominator
            let needAdd1 = u.Units.Head > 4y
            let u = u / Ubint._10 + if needAdd1 then Ubint._1 else Ubint._0
            let r = u.ToString()
            let decimals = decimals - 1u
            let r =
                let i = (int decimals) + 1 - r.Length
                if i > 0 then
                     (if x.Type = Negative then "-" else "") + String.init i (fun _ -> "0") + r
                else (if x.Type = Negative then "-" else "") + r
            if decimals > 0u
            then r.Insert (r.Length - int decimals, ".") |> float
            else r |> float

    [<CompiledName("ToDouble")>]
    member x.ToFloat ([<Optional; DefaultParameterValue(5u)>] decimals) = Frac.ToFloat (x, decimals)

    static member ToString (x: Frac) =
        match x.Type with
        | Zero -> "0"
        | t ->
            if x.Denominator = Ubint._1 then
                if t = Positive then x.Numerator.ToString()
                else x.Numerator.ToString() |> sprintf "-%s"
            else
                (x.Numerator.ToString(), x.Denominator.ToString())
                ||> if t = Positive then sprintf "%s/%s"
                    else sprintf "-(%s/%s)"

    override x.ToString () = Frac.ToString x

    override x.Equals obj =
        if isNull obj then false
        elif obj :? Frac then
            let obj: Frac = downcast obj
            obj.Type = _type &&
            obj.Numerator = x.Numerator &&
            obj.Denominator = x.Denominator
        else false

    interface IComparable with
        member x.CompareTo obj =
            if isNull obj then -1
            elif x.Equals obj then 0
            elif obj :? Frac then
                let y: Frac = downcast obj

                match (_type, y.Type) with
                | Positive, Negative | Positive, Zero | Zero, Negative -> 1
                | Negative, Zero | Negative, Positive | Zero, Positive -> -1
                | Zero, Zero -> 0
                | Positive, Positive | Negative, Negative ->
                    let xn = x.Numerator * y.Denominator
                    let yn = y.Numerator * x.Denominator

                    if _type = Positive then
                        if   xn > yn then 1
                        elif xn = yn then 0
                        else -1
                    elif xn > yn then -1
                    elif xn = yn then 0
                    else 1

            else -1

    static member op_Explicit (numerator, denominator) =
        let r = numerator * denominator
        if r = 0L then Frac._0
        else ((if r > 0L then Positive else Negative), ubintFromInt64 numerator, ubintFromInt64 denominator) |> ctor
    static member op_Explicit num =
        if num = 0L then Frac._0
        else Frac ((if num > 0L then Positive else Negative), num |> ubintFromInt64, Ubint._1)
    static member op_Explicit num = ubintFromFloat num |> ctor
    static member op_Explicit num = ubintFromDecimal num |> ctor

    static member (+) (x, y) =
        if x = Frac._0 then y
        elif y = Frac._0 then x
        else
            let xn = x.Numerator * y.Denominator
            let yn = y.Numerator * x.Denominator
            if x.Type = y.Type then
                let n, d = reduce (xn + yn) (x.Denominator * y.Denominator)
                Frac (x.Type, n, d)
            elif xn > yn then
                let n, d = reduce (xn - yn) (x.Denominator * y.Denominator)
                Frac (x.Type =+&- y.Type, n, d)
            else
                let n, d = reduce (yn - xn) (x.Denominator * y.Denominator)
                Frac (-(x.Type =+&- y.Type), n, d)

    static member (~-) (x: Frac) =
        match x.Type with
        | Negative -> Frac (Positive, x.Numerator, x.Denominator)
        | Positive -> Frac (Negative, x.Numerator, x.Denominator)
        | _ -> x

    static member Abs (x: Frac) =
        match x.Type with
        | Negative -> -x
        | _ -> x

    static member (-) (x, y) =
        if x = Frac._0 then -y
        elif y = Frac._0 then x
        else
            let xn = x.Numerator * y.Denominator
            let yn = y.Numerator * x.Denominator

            if xn = yn then Frac._0
            elif x.Type = y.Type then
                if xn > yn then
                    let n, d = reduce (xn - yn) (x.Denominator * y.Denominator)
                    Frac (x.Type, n, d)
                else
                    let n, d = reduce (yn - xn) (x.Denominator * y.Denominator)
                    Frac (-x.Type, n, d)
            else
                let n, d = reduce (xn + yn) (x.Denominator * y.Denominator)
                Frac (x.Type =-&+ y.Type, n, d)

    static member (*) (x, y) =
        if x = Frac._0 || y = Frac._0 then Frac._0
        elif x = Frac._1 then y
        elif x = Frac._m1 then -y
        elif y = Frac._1 then x
        elif y = Frac._m1 then -x
        else
            let n, d = reduce (x.Numerator * y.Numerator) (x.Denominator * y.Denominator)
            Frac (x.Type =& y.Type, n, d)

    static member (/) (x, y) =
        if y = Frac._0 then failwith "除数不能为零。"
        elif x = Frac._1 then Frac.GetReciprocal y |> Result.unwrap
        elif x = Frac._m1 then Frac.GetReciprocal -y |> Result.unwrap
        elif y = Frac._1 then x
        elif y = Frac._m1 then -x
        else
            let n, d = reduce (x.Numerator * y.Denominator) (x.Denominator * y.Numerator)
            Frac (x.Type =& y.Type, n, d)

    static member Pow (a, n) =
        if   a = Frac._0 then failwith "不支持对0进行幂运算。"
        elif n = 0 then Frac._1
        elif n = 1 then a
        elif n < 0 then (a |> Frac.GetReciprocal |> Result.unwrap, -n) |> Frac.Pow
        else
            let rec cal_opPow a n =
                if n = 1 then a
                else cal_opPow (a * a) (n - 1)
            cal_opPow a n