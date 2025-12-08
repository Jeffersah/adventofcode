module AdventOfCode.Day5

open System
open AdventOfCode.Common

let parseInput (lines: string list) =
    let parseLine (ranges, ingredients) line =
        if line = "" then (ranges, ingredients)
        else match line.IndexOf('-') with
             | -1 -> (ranges, uint64 line :: ingredients)
             | n ->
                let [|min;max|] = line.Split('-')
                ((Range.fromMinMax (uint64 min) (uint64 max))::ranges, ingredients)
    lines
    |> List.fold (parseLine) ([], [])

let run (input: string list) =
    let ranges, ingredients = parseInput input
    let anyRangeMatched ranges value =
        List.exists (fun r -> Range.contains r value) ranges
    let fresh, spoiled = List.partition (anyRangeMatched ranges) ingredients
    List.length fresh
    
    
let rec mergeInto ranges processed range =
    match ranges with
    | [] -> range::processed
    | r::rest ->
        match Range.tryUnion r range with
        | Some r -> mergeInto (processed @ rest) [] r
        | None -> mergeInto rest (r::processed) range

let rec mergeAll ranges merged =
    match ranges with
    | [] -> merged
    | r::rs -> mergeAll rs (mergeInto merged [] r)

let run2 (input: string list) =
    let ranges, _ = parseInput input
    let merged = mergeAll ranges []
    
    merged
    |> List.sumBy (Range.length)
