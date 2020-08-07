open System
open MMath.Core

[<EntryPoint>]
let main _ =
    (Ubint.FromString "791178477", Ubint.FromString "61996252")
    |> Result.unwrap2
    ||> (/)
    |> Ubint.ToString
    |> printfn "%s"
    (Frac.op_Explicit (36L, -3L), -2)
    |> Frac.Pow
    |> Frac.ToString
    |> printfn "%s"
    0