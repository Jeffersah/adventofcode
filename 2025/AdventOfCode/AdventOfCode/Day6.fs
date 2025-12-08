module AdventOfCode.Day6

open System
open AdventOfCode.Common

type Problem = { elements: int64 list; operator: char }

let parseRegion (input: string list) (x: int) (length: int) =
    let elements =
        input
        |> List.map(fun line -> line.Substring(x, length))
    elements

let parseProblem (elements: string list) =
    let op::values = List.rev elements
    let elements = List.rev values |> List.map (fun s -> s.Trim()) |> List.map int64
    { elements = elements; operator = op.Trim()[0] }

let parseInputColumns (input: string list) =
    let maxLength = input |> List.map (String.length) |> List.max
    let padded = input |> List.map (fun s -> s.PadRight(maxLength, ' '))
    let empty =
        [0..maxLength-1]
        |> List.where (fun c -> padded |> List.forall (fun str -> str[c] = ' '))
        
    let empty = empty @ [maxLength]
    
    let rec regions x emptyColumns =
        match emptyColumns with
        | [] -> []
        | c::columns ->
            (x, c-x) :: regions (c+1) columns
    let regions = regions 0 empty
    
    let parsedRegions = regions |> List.map ((<||) (parseRegion padded))
    
    List.map parseProblem parsedRegions

let solveProblem (problem: Problem) =
    match problem.operator with
    | '*' -> List.fold (*) 1L problem.elements
    | '+' -> List.fold (+) 0L problem.elements
    | _ -> failwithf "Unknown symbol %c" problem.operator

// We could just split by spaces and ignore empty entries,
// but I suspect the left/right alignment may be important later.
let run (input: string list) =
    let problems = parseInputColumns input
    problems
    |> List.map solveProblem
    |> List.fold (+) 0L
    
let parseProblem2 (input: string list) =
    let maxLength = input |> List.map (String.length) |> List.max
    let padded = input |> List.map (fun s -> s.PadRight(maxLength, ' '))
    let transposed =
        padded
        |> List.map (List.ofSeq)
        |> List.transpose
        |> List.map (Seq.map string >> String.concat "")
    
    let rec splitIntoProblems transposed =
        match List.tryFindIndex (String.IsNullOrWhiteSpace) transposed with
        | None -> [transposed]
        | Some i ->
            let problem, _::rest = List.splitAt i transposed
            problem :: splitIntoProblems rest
            
    let split = splitIntoProblems transposed
    
    let parseSplitProblem (problem: string list) =
        let withOp::rest = problem
        let op = withOp[withOp.Length - 1]
        let elements = withOp.Substring(0, withOp.Length - 1) :: rest
        { operator = op; elements = elements |> List.map (fun s -> int64 (s.Trim()))  }
    
    List.map parseSplitProblem split
    
    
let run2 (input: string list) =
    parseProblem2 input
    |> List.map solveProblem
    |> List.fold (+) 0L