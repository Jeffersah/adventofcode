module AdventOfCode.Day11

open System.Collections.Generic
open AdventOfCode.Common


let stepOne (stone: uint64) =
    match stone with
    | 0UL -> [ 1UL ]
    | x when Digits.digitCount x % 2 = 0 ->
        let digitsLe = Digits.digitsLE x |> List.ofSeq
        let [ secondLe; firstLe ] = List.splitInto 2 digitsLe
        [ Digits.uint64FromDigitsBE (List.rev firstLe); Digits.uint64FromDigitsBE (List.rev secondLe) ] 
    | x -> [ x * 2024UL ]
    
let stepAll = List.collect stepOne

let run (input: string list) =
    let stones = input[0].Split(' ') |> Seq.map uint64 |> List.ofSeq
    
    let rec stepNTimes stones left =
        if left <= 0 then stones
        else stepNTimes (stepAll stones) (left - 1)
    
    stepNTimes stones 25
    |> List.length
    
// For part 2:
// We note that order doesn't actually matter.
// Instead of storing a list of stones, store a counting set of stones.
// All stones of the same value update in the same way.  

let stepAllCounts =
    Counts.bind (stepOne >> Counts.ofSeq)

let run2_test (input: string list) =
    let stones = input[0].Split(' ') |> Seq.map uint64 |> List.ofSeq |> Counts.ofSeq
    
    let rec stepNTimes stones left =
        if left <= 0 then stones
        else
            printfn "%3i: (%i) %A" left (Counts.count stones) (stones |> Counts.toCountSeq |> Seq.sortByDescending snd)
            stepNTimes (stepAllCounts stones) (left - 1)
    
    stepNTimes stones 25
    |> Counts.count

let run2 (input: string list) =
    let stones = input[0].Split(' ') |> Seq.map uint64 |> List.ofSeq |> Counts.ofSeq
    
    let rec stepNTimes stones left =
        if left <= 0 then stones
        else stepNTimes (stepAllCounts stones) (left - 1)
    
    stepNTimes stones 75
    |> Counts.count