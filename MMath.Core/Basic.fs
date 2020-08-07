namespace MMath.Core

[<System.Diagnostics.DebuggerStepThrough>]
module Result =
    let unwrap x =
        match x with
        | Ok r -> r
        | Error e -> failwith e
    
    let unwrap2 (x, y) = unwrap x, unwrap y
    
    let unwrap3 (x, y, z) = unwrap x, unwrap y, unwrap z