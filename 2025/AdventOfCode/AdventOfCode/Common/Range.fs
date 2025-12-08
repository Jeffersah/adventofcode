namespace AdventOfCode.Common

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type Range<'a> = { start: 'a; length: 'a }

module Range =
    let inline create start length = { start = start; length = length }
    let inline fromMinMax min max = { start = min; length = max - min + GenericMath.ofInt 1 }
    let inline tryFromMinMax min max =
        if max < min then None else Some(fromMinMax min max)
    let inline contains { start = start; length = length } value =
        value >= start && value < start + length
    
    let inline isEmpty { length = length } = length = GenericMath.ofInt 0
    let inline length { length = length } = length
    let inline start { start = start } = start
    let inline tryGetFirst { start = start; length = len } =
        if len >= GenericMath.ofInt 1 then Some start else None
    let inline tryGetLast { start = start; length = len } =
        if len >= GenericMath.ofInt 1 then Some (start + len - GenericMath.ofInt 1) else None
    let inline tryMinMax { start = start; length = length } =
        if length >= GenericMath.ofInt 1 then
            Some(start, start + length - GenericMath.ofInt 1)
        else
            None
    let inline isSuperset outer inner =
        match tryMinMax inner with
        | None -> true
        | Some (min, max) -> contains outer min && contains outer max
    let inline isStrictSuperset outer inner =
        outer <> inner && isSuperset outer inner
    let inline isSubset inner outer = isSuperset outer inner
    let inline isStrictSubset inner outer = isStrictSuperset outer inner
    let inline intersection (a: 'a Range) (b: 'a Range) =
        match (tryMinMax a), (tryMinMax b) with
        | Some(mina, maxa), Some(minb, maxb) ->
            let left = max mina minb
            let right = min maxa maxb
            tryFromMinMax left right
        | _, None -> Some b
        | None, _ -> Some a
    let inline union (a: ^a Range) (b: ^a Range) : ^a Range =
        match (tryMinMax a), (tryMinMax b) with
        | Some(mina, maxa), Some(minb, maxb) ->
            let left = min mina minb
            let right = max maxa maxb
            fromMinMax left right
        | _, None -> b
        | None, _ -> a
    let inline tryUnion a b =
        intersection a b
        |> Option.map (fun _ -> union a b)