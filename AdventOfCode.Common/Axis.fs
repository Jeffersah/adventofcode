namespace AdventOfCode.Common

open System.Runtime.CompilerServices
open AdventOfCode.Common

[<IsReadOnly; Struct>]
type Axis =
    private Axis of byte
with
    static member X = Axis.Axis 0uy
    static member Y = Axis.Axis 1uy
    static member Z = Axis.Axis 2uy
    static member ofNthDimension n = Axis.Axis n
    override this.ToString() : string =
        match this with
        | Axis.Axis 0uy -> "X"
        | Axis.Axis 1uy -> "Y"
        | Axis.Axis 2uy -> "Z"
        | Axis.Axis 3uy -> "W"
        | Axis.Axis v -> sprintf "Axis(%i)" v
        
module Axis =
    let (|X|Y|Z|W|Nth|) (Axis.Axis v) =
        match v with
        | 0uy -> X
        | 1uy -> Y
        | 2uy -> Z
        | 3uy -> W
        | v -> Nth v
        
[<IsReadOnly; Struct>]
type Facing = { axis: Axis; positive: bool }

module Facing =
    let positive axis = { axis = axis; positive = true }
    let negative axis = { axis = axis; positive = false }
    let opposite { axis = axis; positive = positive } = { axis = axis; positive = not positive }
    
    