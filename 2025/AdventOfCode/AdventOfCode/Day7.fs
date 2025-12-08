module AdventOfCode.Day7

open System
open AdventOfCode
open AdventOfCode.Common

let parseInput (input: string list)=
    let asCharMap = Parsing.linesToCharMap2d input
    let start = Map.findKey (fun _ v -> v = 'S') asCharMap
    Map.filter (fun k v -> v = '^') asCharMap
    |> Map.keys
    |> Seq.map (fun pt -> Point.sub pt start)
    |> Set.ofSeq
    
let run (input: string list) =
    let splitters = parseInput input
    
    // We really don't care about Y, the rows just need to be in order.
    let splitterSets =
        splitters
        |> Seq.groupBy Point.Y // Group splitters by row
        |> Seq.sortBy (fst) // Sort by Y
        |> Seq.map snd // Get just the splitters
        |> Seq.map (Seq.map Point.X >> Set.ofSeq) // Get just the X coords
        |> List.ofSeq // As a list
        
    // splitterSets looks like [[0]; [-1; 1]; [-2; 0; 2]; ...]
        
    let rec advanceBeam beamPositions splitterSets splitCount =
        match splitterSets with
        | [] -> beamPositions, splitCount
        | splitters::splitterSets ->
            let hits = Set.intersect beamPositions splitters
            let misses = Set.difference beamPositions splitters
            let newSplitCount = splitCount + Set.count hits
            let newBeams =
                Set.unionMany [
                    hits |> Set.map (fun n -> n + 1);
                    hits |> Set.map (fun n -> n - 1);
                    misses
                ]
            advanceBeam newBeams splitterSets newSplitCount
    let beam, split = advanceBeam (Set.ofList [0]) splitterSets 0
    beam, split
                
    
let run2 (input: string list) =
    let splitters = parseInput input
    
    // We really don't care about Y, the rows just need to be in order.
    let splitterSets =
        splitters
        |> Seq.groupBy Point.Y // Group splitters by row
        |> Seq.sortBy (fst) // Sort by Y
        |> Seq.map snd // Get just the splitters
        |> Seq.map (Seq.map Point.X >> Set.ofSeq) // Get just the X coords
        |> List.ofSeq // As a list
        
    // splitterSets looks like [[0]; [-1; 1]; [-2; 0; 2]; ...]
        
    let rec advanceBeam (beamPositions: int Counts) splitterSets =
        match splitterSets with
        | [] -> beamPositions
        | splitters::splitterSets ->
            let hits = Counts.setOperation Set.intersect splitters beamPositions
            let misses = Counts.setOperation Set.difference splitters beamPositions
            let newBeams =
                Counts.union
                    (hits |> Counts.map (fun x -> x + 1))
                    (hits |> Counts.map (fun x -> x - 1))
                |> Counts.union
                    misses
            advanceBeam newBeams splitterSets
    let beam = advanceBeam (Counts.ofSeq [0]) splitterSets
    Counts.count beam
