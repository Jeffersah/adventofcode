namespace AdventOfCode.Common


module Parsing =
    let linesToCharMap2d (lines: string list) : Map<int Point, char> =
        lines
        |> List.map (Seq.indexed)
        |> Seq.indexed
        |> Seq.fold
            (fun map (y, elements) ->
                Seq.fold (fun map (x, chr) -> Map.add { x = x; y = y } chr map) map elements
            )
            Map.empty