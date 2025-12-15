open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day9_sample.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day9.run input)