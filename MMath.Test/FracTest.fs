module MMath.FracTest

open FsUnit
open MMath.Core
open NUnit.Framework

let random = System.Random()
let nums1 = [ 
        0m
        1m
        1m
        1m
        2m
        for _ = 0 to 994 do random.Next 9999999 |> decimal ]
let nums2 = [ 
        0m
        0m
        1m
        10m
        2m
        for _ = 0 to 994 do random.Next 9999999 |> decimal ]

let (|>>) (x: decimal list, y: decimal list) f = (x |> List.map Frac.op_Explicit, y |> List.map Frac.op_Explicit) ||> f

[<Test>]
let ``= Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (=)
    let had    = (nums1, nums2) |>> List.map2 (=)
    had |> should equal needed

[<Test>]
let ``> Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (>)
    let had    = (nums1, nums2) |>> List.map2 (>)
    had |> should equal needed

[<Test>]
let ``< Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (<)
    let had    = (nums1, nums2) |>> List.map2 (<)
    had |> should equal needed

[<Test>]
let ``+ Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (+) |> List.map Frac.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (+)
    had |> should equal needed

[<Test>]
let ``- Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (-) |> List.map Frac.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (-)
    had |> should equal needed
    
[<Test>]
let ``* Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (*) |> List.map Frac.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (*)
    (needed, had) ||> List.iteri2 (fun i j k -> if j <> k then printfn "%f * %f should = %s but has %s" nums1.[i] nums2.[i] (j.ToString()) (k.ToString()) )
    had |> should equal needed


[<Test>]
let ``/ Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (fun i j -> if j = 0m then 0m else i / j) |> List.map (fun i -> System.Math.Round (i, 5) |> float)
    let had    = (nums1, nums2) |>> List.map2 (fun i j -> if j = Frac._0 then Frac._0 else i / j) |> List.map (Frac.ToFloat)
    (needed, had) ||> List.iteri2 (fun i j k -> if j <> k then printfn "%f / %f should = %f but has %f" nums1.[i] nums2.[i] j k)
    had |> should equal needed