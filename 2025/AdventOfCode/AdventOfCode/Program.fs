open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day10_sample.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day10.run2 input)