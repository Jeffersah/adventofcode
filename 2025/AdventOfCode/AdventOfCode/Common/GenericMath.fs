module AdventOfCode.Common.GenericMath

let inline ofInt<^a when (^a or int) : (static member op_Explicit : int -> ^a)> (i: int) =
    ((^a or int) : (static member op_Explicit : int -> ^a) (i))