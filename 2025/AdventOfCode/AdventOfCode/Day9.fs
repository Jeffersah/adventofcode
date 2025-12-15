module AdventOfCode.Day9

open System
open System.Drawing
open System.Net.Http.Headers
open System.Reflection
open System.Runtime.CompilerServices
open AdventOfCode
open AdventOfCode.Common

let parseInput (input: string list)=
    let parseOne (line: string) =
        let [| x; y |] = line.Split(',')
        Point.create (int64 x) (int64 y)
    List.map parseOne input

let run (input: string list) =
    let points = parseInput input
    List.combinations points
    |> List.map (fun (a, b) -> Rect.ofBounds a b |> Rect.area)
    |> List.max
