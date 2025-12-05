open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day11.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day11.run2 input)