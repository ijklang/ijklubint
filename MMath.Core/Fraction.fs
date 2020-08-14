namespace MMath.Core

open System
open System.Diagnostics
open System.Text.RegularExpressions
open System.Runtime.InteropServices

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
module internal fracHlpr =
    let rec private gdc a b = if b = Ubint._0 then a else gdc b (a % b)

    let reduce numerator denominator =
        if numerator = denominator then Ubint._1, Ubint._1
        elif numerator = Ubint._0 then Ubint._0, Ubint._1
        elif numerator = Ubint._1 || denominator = Ubint._1 then numerator, denominator
        else
            let r =
                if numerator > denominator then gdc numerator denominator
                else gdc denominator numerator
            numerator / r, denominator / r
    let ubintFromInt64 num = num |> abs |> uint64 |> Ubint.op_Explicit

    let zero   = Regex(@"^(\-)?0+(\.0+)?$")
    let Int    = Regex(@"^\-?\d+?$")
    let Float  = Regex(@"^\-?\d+?\.\d+?$")
    let FloatE = Regex(@"^\-?\d(\.\d+)?E\d+?$")

    let rec ubintFromString input =
        if  zero.IsMatch input then Ok (Zero, Ubint._0, Ubint._1)
        elif Int.IsMatch input then
            let isNeg = input.[0] = '-'
            Ok ((if isNeg then Negative else Positive),
                Ubint.FromString input.[(if isNeg then 1 else 0)..] |> Result.unwrap,
                Ubint._1)
        elif Float.IsMatch input then
            let fullNum = input.Replace (".", "")
            let 小数位数 = input.Length - 1 - input.IndexOf '.'
            let powOf10 = "1" + String.init 小数位数 (fun _ -> "0")
            let isNeg = input.[0] = '-'
            Ok ((if isNeg then Negative else Positive),
                Ubint.FromString fullNum.[(if isNeg then 1 else 0)..] |> Result.unwrap,
                Ubint.FromString powOf10 |> Result.unwrap)
        elif FloatE.IsMatch input then
            let 要乘十的几次方 = Int32.Parse input.[input.IndexOf '+' + 1..]
            let powOf10 = "1" + String.init (abs 要乘十的几次方) (fun _ -> "0")
            let powOf10 = Ubint.FromString powOf10 |> Result.unwrap
            let baseNum = ubintFromString input.[0..input.IndexOf 'E' - 1]
            match baseNum with
            | Ok (t, n, d) ->
                let n, d =
                    if 要乘十的几次方 < 0
                    then           n, d * powOf10
                    else n * powOf10, d
                Ok (t, n, d)
            | Error e -> Error e
        else Error <| sprintf "无法识别字符串“%s”。" input
    let ubintFromFloat   (n: float) =   n.ToString() |> ubintFromString |> Result.unwrap
    let ubintFromDecimal (n: decimal) = n.ToString() |> ubintFromString |> Result.unwrap

// "此构造已弃用。"
#nowarn "44"
open fracHlpr
#warn "44"

[<CompiledName "Fraction";>]
type Frac private (_type, _numerator, _denominator) =
    static let make (t, n, d) =
        let n, d= reduce n d
        Frac (t, n, d)

    static let rec cal_opPow a n =
        match n with
        | 1 -> a
        | _ -> cal_opPow (a * a) (n - 1)

    member _.Type        with get() = _type
    member _.Numerator   with get() = _numerator
    member _.Denominator with get() = _denominator

    [<CompiledName "Zero">]
    static member _0  = Frac (Zero, Ubint._0, Ubint._1)
    [<CompiledName "One">]
    static member _1  = Frac (Positive, Ubint._1, Ubint._1)
    [<CompiledName "MinusOne">]
    static member _m1 = Frac (Negative, Ubint._1, Ubint._1)

    /// n: 要保留的位数
    [<CompiledName("ToDouble")>]
    static member ToFloat (x: Frac, [<Optional; DefaultParameterValue(5u)>] decimals) =
        match x.Type with
        | Zero -> 0.
        | _ ->
            let decimals' = decimals + 1u
            let u = x.Numerator.MulByPowOf10 decimals' / x.Denominator
            let r =
                if u.Units.Head > 4y
                then u / Ubint._10 + Ubint._1
                else u / Ubint._10
                |> Ubint.ToString
            let r =
                let i = (int decimals') - r.Length
                if i > 0 then
                    if x.Type = Negative
                    then "-" + String.init i (fun _ -> "0") + r
                    else String.init i (fun _ -> "0") + r
                elif x.Type = Negative
                then "-" + r
                else r
            if decimals > 0u
            then Double.Parse <| r.Insert (r.Length - int decimals, ".")
            else Double.Parse r
    [<CompiledName("ToDouble")>]
    member x.ToFloat ([<Optional; DefaultParameterValue(5u)>] decimals) = Frac.ToFloat (x, decimals)

    static member ToString (x: Frac) =
        match x.Type with
        | Zero -> "0"
        | t ->
            if x.Denominator = Ubint._1 then
                if t = Positive then x.Numerator.ToString()
                else sprintf "-%s" <| x.Numerator.ToString()
            else
                if t = Positive
                then sprintf "%s/%s"
                else sprintf "-(%s/%s)"
                <|| (x.Numerator.ToString(), x.Denominator.ToString())
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

    static member From (ftype, numer, denom) =
        if denom = Ubint._0
        then Error "分母不能为0。"
        elif (ftype = Zero && numer <> Ubint._0) || (numer = Ubint._0 && ftype <> Zero)
        then Error <| sprintf "type “%A”与分数值不匹配" ftype
        else Ok <| make (ftype, numer, denom)
    static member From (ftype, num) = Frac.From (ftype, num, Ubint._1)
    static member From str =
        match ubintFromString str with
        | Ok    r -> Ok <| make r
        | Error e -> Error e

    static member op_Explicit (numer, denom) =
        match numer * denom with
        | 0L -> Frac._0
        | r -> make ((if r > 0L then Positive else Negative), ubintFromInt64 numer, ubintFromInt64 denom)
    static member op_Explicit num =
        match num with
        | 0L -> Frac._0
        | _ -> Frac ((if num > 0L then Positive else Negative), num |> ubintFromInt64, Ubint._1)
    static member op_Explicit num = ubintFromFloat   num |> make
    static member op_Explicit num = ubintFromDecimal num |> make

    static member GetReciprocal (x: Frac) =
        match x.Type with
        | Zero -> Error "0没有倒数。"
        | _ -> Ok <| make (x.Type, x.Denominator, x.Numerator)

    static member (+) (x, y) =
        if   x = Frac._0 then y
        elif y = Frac._0 then x
        else
            let xn = x.Numerator * y.Denominator
            let yn = y.Numerator * x.Denominator
            let d = x.Denominator * y.Denominator
            make
            <|  if x.Type = y.Type then          x.Type, xn + yn, d
                elif xn < yn then -(x.Type =+&- y.Type), yn - xn, d
                else                 x.Type =+&- y.Type, xn - yn, d

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
        if   x = Frac._0 then -y
        elif y = Frac._0 then x
        else
            let xn = x.Numerator * y.Denominator
            let yn = y.Numerator * x.Denominator

            if xn = yn then Frac._0
            else
                let d = x.Denominator * y.Denominator
                make
                <|  if x.Type = y.Type then
                        if xn > yn then x.Type, xn - yn, d
                        else           -x.Type, yn - xn, d
                    else    x.Type =-&+ y.Type, xn + yn, d

    static member (*) (x, y) =
        if x = Frac._0 || y = Frac._0 then Frac._0
        elif x = Frac._1 then y
        elif x = Frac._m1 then -y
        elif y = Frac._1 then x
        elif y = Frac._m1 then -x
        else make (x.Type =& y.Type, x.Numerator * y.Numerator, x.Denominator * y.Denominator)

    static member (/) (x, y) =
        if   y = Frac. _0 then failwith "除数不能为零。"
        elif y = Frac. _1 then  x
        elif y = Frac._m1 then -x
        elif x = Frac. _1 then Frac.GetReciprocal  y |> Result.unwrap
        elif x = Frac._m1 then Frac.GetReciprocal -y |> Result.unwrap
        else make (x.Type =& y.Type, x.Numerator * y.Denominator, x.Denominator * y.Numerator)

    static member Pow (a, n) =
        if   a = Frac._0 then failwith "不支持对0进行幂运算。"
        elif n < 0 then (Frac.GetReciprocal a |> Result.unwrap) ** -n
        elif n = 0 then Frac._1
        elif n = 1 then a
        else cal_opPow a n