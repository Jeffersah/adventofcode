module AdventOfCode.Common.List

let combinations list =
    let rec combinationsRec list acc =
        match list with
        | [] -> acc
        | x::r ->
            acc @ List.map (fun y -> (x, y)) r
            |> combinationsRec r
    combinationsRec list []
    
let zipShort a b =
    let rec zipShortRec a b acc =
        match struct(a, b) with
        | struct([], x)
        | struct(x, []) -> acc
        | struct(x::xs, y::ys) -> zipShortRec xs ys ((x, y)::acc)
    zipShortRec a b []
    |> List.rev