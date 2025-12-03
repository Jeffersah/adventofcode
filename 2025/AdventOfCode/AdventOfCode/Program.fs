open System.IO
open AdventOfCode

let inputFolder = "../../../../../Input/"

let inputFile = "day3.txt"

let input = File.ReadAllLines(inputFolder + inputFile) |> List.ofArray

printfn "%A" (Day3.run3 input)