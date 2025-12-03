open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day2.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day2.Part2.run2 input)