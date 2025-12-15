module AdventOfCode.Common.List

let combinations list =
    let rec combinationsRec list acc =
        match list with
        | [] -> acc
        | x::r ->
            acc @ List.map (fun y -> (x, y)) r
            |> combinationsRec r
    combinationsRec list []