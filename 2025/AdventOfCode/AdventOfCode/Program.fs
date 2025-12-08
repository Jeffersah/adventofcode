open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day5.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day5.run2 input)