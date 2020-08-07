module MMath.UbintTest

open FsUnit
open MMath.Core
open NUnit.Framework

let random = System.Random()
let nums1 = [ 
        0L
        1L
        1L
        1L
        2L
        for _ = 0 to 994 do random.Next 9999999 |> int64 ]
let nums2 = [ 
        0L
        0L
        1L
        10L
        2L
        for _ = 0 to 994 do random.Next 9999999 |> int64 ]

let (|>>) (x: int64 list, y: int64 list) f = (x |> List.map Ubint.op_Explicit, y |> List.map Ubint.op_Explicit) ||> f

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
    let needed = (nums1, nums2) ||> List.map2 (+) |> List.map Ubint.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (+)
    had |> should equal needed

[<Test>]
let ``- Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (fun i j -> if i < j then 0L else i - j) |> List.map Ubint.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (fun i j -> try i - j with _ -> Ubint._0)
    had |> should equal needed
    
[<Test>]
let ``* Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (*) |> List.map Ubint.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (*)
    (needed, had) ||> List.iteri2 (fun i j k -> if j <> k then printfn "%i * %i should = %s but has %s" nums1.[i] nums2.[i] (j.ToString()) (k.ToString()) )
    had |> should equal needed


[<Test>]
let ``/ Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (fun i j -> if j = 0L then 0L else (i / j)) |> List.map Ubint.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (fun i j -> try i / j with _ -> Ubint._0)
    (needed, had) ||> List.iteri2 (fun i j k -> if j <> k then printfn "%i / %i should = %s but has %s" nums1.[i] nums2.[i] (j.ToString()) (k.ToString()) )
    had |> should equal needed

[<Test>]
let ``% Test`` () =
    let needed = (nums1, nums2) ||> List.map2 (fun i j -> if j = 0L then 0L else (i % j)) |> List.map Ubint.op_Explicit
    let had    = (nums1, nums2) |>> List.map2 (fun i j -> try i % j with _ -> Ubint._0)
    (needed, had) ||> List.iteri2 (fun i j k -> if j <> k then printfn "%i %% %i should = %s but has %s" nums1.[i] nums2.[i] (j.ToString()) (k.ToString()) )
    had |> should equal needed

[<Test>]
let ``IsPowOf10 Test`` () =
    let rec test (num, state) = 
        if num = 0L then false
        elif num = 1L then state
        else test (num / 10L, (num % 10L = 0L) && state)

    let needed = nums1 |> List.map (fun i -> test (i, true))
    let had    = nums1 |> List.map (fun i -> (Ubint.op_Explicit i).IsPowOf10)
    had |> should equal needed