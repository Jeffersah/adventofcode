open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day6.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day6.run2 input)