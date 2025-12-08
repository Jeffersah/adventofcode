open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day8.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day8.run2 input)