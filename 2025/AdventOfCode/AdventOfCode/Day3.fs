module AdventOfCode.Day3

open System
open System.Runtime.InteropServices

let rec parseLine (input: string) =
    input
    |> Seq.map (fun ch -> int (ch - '0'))
    |> List.ofSeq

// Very simple brute-force works for part 1
let rec bestPower batteries countLeft power =
    if countLeft = 0 then power
    else
        match batteries with
        | [] -> 0UL
        | b::bs ->
            let bestWith = bestPower bs (countLeft - 1) (power * 10UL + uint64 b)
            let bestWithout = bestPower bs countLeft power
            max bestWith bestWithout

let run (input: string list) =
    let banks = List.map parseLine input
    let mutable sum = 0UL
    for bank in banks do
        printfn "%A" bank
        let power = (bestPower bank 2 0UL)
        printfn "%i" power
        sum <- sum + power
    sum

// Slightly more optimized, prunes branches that are definitely worse than previously-seen branches
let rec bestPower2 batteries countLeft bestSoFar maxLeft power =
    if countLeft = 0 then power
    elif maxLeft < bestSoFar then 0UL
    else
        match batteries with
        | [] -> 0UL
        | b::bs ->
            let place = uint64 (Math.Pow(10, float (countLeft - 1)))
            let maxLeftWithThis = maxLeft - (9UL * place) + (uint64 b * place)
            let bestWith = bestPower2 bs (countLeft - 1) bestSoFar maxLeftWithThis (power * 10UL + uint64 b)
            let bestWithout = bestPower2 bs countLeft (max bestSoFar bestWith) maxLeft power
            max bestWith bestWithout    

let run2 (input: string list) =
    let banks = List.map parseLine input
    let mutable sum = 0UL
    for bank in banks do
        printfn "%A" bank
        let power = (bestPower2 bank 12 0UL 999999999999UL 0UL)
        printfn "%i" power
        sum <- sum + power
    sum
    
// But there's a much better approach:
// Always start with 9 if possible. Otherwise 8, then 7, then 6 ...
//      and recurse from there.
let rec findStartDigit batteries countLeft n =
    if List.length batteries < countLeft then None
    else
        match batteries with
        | [] -> None
        | b::bs when b = n ->
            Some bs
        | _::bs ->
            findStartDigit bs countLeft n
    
let rec bestPower3 batteries countLeft acc =
    if countLeft = 0 then acc
    else
        let digits = List.rev [0..9]
        let (digit, remaining) =
            digits |> List.pick (fun n -> findStartDigit batteries countLeft n |> Option.map (fun b -> n, b))
        bestPower3 remaining (countLeft - 1) (acc * 10UL + uint64 digit)
    
let run3 (input: string list) =
    let banks = List.map parseLine input
    let mutable sum = 0UL
    for bank in banks do
        printfn "%A" bank
        let power = (bestPower3 bank 12 0UL)
        printfn "%i" power
        sum <- sum + power
    sum