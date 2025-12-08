namespace AdventOfCode2024.Common

type Counts<'t when 't : comparison> =
    private Counts of Map<'t, uint64>

module Counts =
    let empty = Counts (Map.empty)
    
    let add element (Counts counts) =
        Counts (Map.change element (Option.map ((+) 1UL) >> Option.defaultValue 1UL >> Some) counts)
    
    let remove element (Counts counts) =
        Counts (Map.change element (Option.filter (fun x -> x > 1UL) >> Option.map (fun x -> x - 1UL)) counts)
        
    let addMany count element (Counts counts) =
        Counts (Map.change element (Option.map ((+) count) >> Option.defaultValue count >> Some) counts)
    
    let removeMany count element (Counts counts) =
        Counts (Map.change element (Option.filter (fun x -> x > count) >> Option.map (fun x -> x - count)) counts)
        
    let removeAll element (Counts counts) = Counts(Map.remove element counts)

    let countOf element (Counts counts) = Map.tryFind element counts |> Option.defaultValue 0UL
    
    let distinctCount (Counts c) = Map.count c
    let count (Counts c) = Map.values c |> Seq.sum
    
    let toMap (Counts c) = c
    let ofMap = Counts
    
    let toCountSeq (Counts c) = Map.toSeq c
    let toSeq (Counts c) = Map.toSeq c |> Seq.collect (fun (v, c) -> Seq.replicate (int c) v)
    
    let ofSeq s = Seq.fold (fun c s -> add s c) empty s
    
    let map fn counts =
        toCountSeq counts
        |> Seq.map (fun (e, c) -> fn e, c)
        |> Seq.fold (fun s (e, c) -> addMany c e s) empty
        
    let bind fn counts =
        toCountSeq counts
        |> Seq.collect (fun (e, c) -> fn e |> toCountSeq |> Seq.map (fun (ne, nc) -> (ne, nc * c)))
        |> Seq.fold (fun s (e, c) -> addMany c e s) empty