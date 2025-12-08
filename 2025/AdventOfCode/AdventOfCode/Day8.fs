module AdventOfCode.Day8

open System
open System.Runtime.CompilerServices
open AdventOfCode
open AdventOfCode.Common

let parseInput (input: string list)=
    let parseOne (line: string) =
        let [| x; y; z |] = line.Split(',')
        Point3.create (int x) (int y) (int z)
    List.map parseOne input

let rec join a b (circuits: Map<'a, 'a Set>) =
    let aCircuit =
        Map.tryFind a circuits
        |> Option.defaultValue (Set.singleton a)
        
    if Set.contains b aCircuit then
        None
    else
        let bCircuit =
            Map.tryFind b circuits
            |> Option.defaultValue (Set.singleton b)
            
        let joined = Set.union aCircuit bCircuit
        
        Some(
            joined
            |> Set.toSeq
            |> Seq.fold (fun map element -> Map.add element joined map) circuits)
    
let rec pairs junctionBoxes =
    match junctionBoxes with
    | [] -> []
    | x::xs ->
        (xs
        |> List.map (fun e -> x, e))
        @ pairs xs
    
let run (input: string list) =
    let junctionBoxes = parseInput input
    let pairs =
        pairs junctionBoxes
        |> List.sortBy (fun (a, b) -> Point3.actualLen (Point3.sub a b))
    
    let joinCount = if List.length input <= 25 then 10 else 1000
    
    let pairs = List.take joinCount pairs
    
    let circuits =
        pairs
        |> List.fold (fun circuits (a, b) -> join a b circuits |> Option.defaultValue circuits) Map.empty
        |> Map.values
        |> Seq.distinct
        
    circuits
        |> Seq.map (Set.count >> uint64)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.fold (*) 1UL


let run2 (input: string list) =
    let junctionBoxes = parseInput input
    let pairs =
        pairs junctionBoxes
        |> List.sortBy (fun (a, b) -> Point3.actualLen (Point3.sub a b))
    
    let rec findLastJoin last pairs circuits =
        match pairs with
        | [] -> last
        | (a, b)::ps ->
            match join a b circuits with
            | None -> findLastJoin last ps circuits
            | Some joined -> findLastJoin (Some (a, b)) ps joined
    
    match findLastJoin None pairs Map.empty with
    | None -> failwith "Something went wrong"
    | Some (a, b) ->
        (uint64 a.x) * (uint64 b.x), (a, b)
