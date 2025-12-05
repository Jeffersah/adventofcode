module AdventOfCode.Day4
open AdventOfCode.Common

let parseInput =
    Parsing.linesToCharMap2d
    >> Map.filter (fun _ v -> v = '@')
    >> Map.keys
    >> Set.ofSeq

let isReachable set point =
    let neighbors =
        Point.octals
        |> Seq.map (Point.add point)
        |> Seq.where (fun pt -> Set.contains pt set)
        |> Seq.length
    neighbors < 4

let run (input: string list) =
    let pointSet = parseInput input
    
    let reachablePoints =
        pointSet
        |> Set.filter (isReachable pointSet)
        
    Set.count reachablePoints
    
let run2 (input: string list) =
    let pointSet = parseInput input
    
    let rec removeUnreachable set count =
        let reachable = Set.filter (isReachable set) set
        let newCount = Set.count reachable
        if newCount = 0 then count
        else removeUnreachable (Set.difference set reachable) (count + newCount)
    
    removeUnreachable pointSet 0