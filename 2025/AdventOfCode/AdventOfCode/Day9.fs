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

let testRect edges { x = x; y = y; w = w; h = h } =
    if w <= 2L || h <= 2L then false // We're going to hope for now no 1xN or 2xN solution is best: our edge detection only works for edge length >= 3
    else
        let inner = { x = x + 1L; y = y + 1L; w = w - 2L; h = h - 2L }
        match List.tryFind (fun test -> Rect.intersection test inner |> Option.isSome) edges with
        | None ->
            // printfn "Ok: (%i, %i)x(%i, %i)" x y w h
            true
        | Some {x = ex; y = ey; w = ew; h = eh} ->
            // printfn "No: (%i, %i)x(%i, %i) hits (%i, %i)x(%i, %i)" x y w h ex ey ew eh
            false

let run2 (input: string list) =
    let points = parseInput input
    let cyclic =
        let x::xs = points
        x :: (xs @ [x])
    let edges =
        List.windowed 2 cyclic
        |> List.map (fun [ a; b ] -> Rect.ofBounds a b)
        
    let rec findBest bestSoFar candidates =
        match candidates with
        | [] -> bestSoFar
        | (a, b)::xs ->
            let region = Rect.ofBounds a b
            let area = Rect.area region
            match bestSoFar with
            | Some n when n >= area -> findBest bestSoFar xs
            | _ ->
                if testRect edges region then
                    findBest (Some area) xs
                else
                    findBest bestSoFar xs
    
    match findBest None (List.combinations points) with
    | None -> failwith "No candidates?"
    | Some n -> n
    