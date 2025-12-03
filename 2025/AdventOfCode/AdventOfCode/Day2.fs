module AdventOfCode.Day2

let parseLine (input: string) =
    let regex = new System.Text.RegularExpressions.Regex("^(\d+)-(\d+)$")
    let mat = regex.Match input
    uint64 mat.Groups[1].Value, uint64 mat.Groups[2].Value

let log10 (number: uint64) =
    let rec log10 number acc =
        if number >= 10UL then
            log10 (number / 10UL) (acc+1)
        else
            acc
    log10 number 1
    
let rec pow10 (n: uint64) pow =
    if pow = 0 then n
    else pow10 (n * 10UL) (pow - 1)
    
let splitHalves number =
    let length = log10 number
    if length % 2 = 1 || length <= 1 then
        None
    else
        let halfLength = length / 2
        let mask = pow10 1UL (halfLength)
        let left = number / mask
        let right = number % mask
        Some(left, right)

let mergeHalves left right =
    let length = log10 right
    pow10 left length + right

let nextInvalidIdOfSameLength (value: uint64) =
    match splitHalves value with
    | None -> None
    | Some (left, right) ->
        if left >= right then
            // 243 | 123 -> 243 | 243
            let length = log10 value
            let halfLength = length / 2
            Some <| mergeHalves left left
        else
            // 11 | 19 -> 12 | 12
            let nl = left + 1UL
            let oldLen = log10 left
            let newLen = log10 nl
            if oldLen <> newLen then None
            else Some <| mergeHalves nl nl
        
let nextInvalidId value =
    match nextInvalidIdOfSameLength value with
    | Some n -> n
    | None ->
        // Lengthen by 1 digit.
        // After doing so, the next invalid id will always be 10...|10...
        let newLength =
            match log10 value with
            | n when n % 2 = 0 -> n + 2
            | n when n % 2 = 1 -> n + 1
        let halfNewLength = newLength / 2
        let halfValue = pow10 1UL (halfNewLength-1)
        mergeHalves halfValue halfValue

let rec invalidsInRange min max =
    let next = nextInvalidId min
    if next <= max then
        next :: invalidsInRange (next+1UL) max
    else
        []

let run (input: string list) =
    let parsed = List.map parseLine input
    
    let invalids = List.collect (fun (min, max) -> invalidsInRange min max) parsed
    
    for (min, max) in parsed do
        printfn "%i-%i" min max
        printfn "%A" (invalidsInRange min max)
    
    List.sum invalids

module Part2 =
    
    type WindowInfo = { windowSize: int; maxSegment: uint64; minSegment: uint64 }
    with
        static member create windowSize =
            let max = (pow10 1UL windowSize) - 1UL
            let min = (pow10 1UL (windowSize - 1))
            { windowSize = windowSize; maxSegment = max; minSegment = min }
    
    let rec nextInvalidId (window: WindowInfo) (segments: uint64 list) =
        match segments with
        | []
        | [_] -> [window.minSegment; window.minSegment]
        | first::rest ->
            let rec tryAdvance first rest =
                match rest with
                | [] -> Some []
                | r::rs when r = first ->
                    tryAdvance first rs
                    |> Option.map (fun res -> r :: res)
                | r::rs when r < first ->
                    Some(List.map (fun _ -> first) rest)
                | _ -> None
                
            match tryAdvance first rest with
            | Some rest -> first :: rest
            | None ->
                if first < window.maxSegment then
                    let nf = first + 1UL
                    nf :: List.map (fun _ -> nf) rest
                else
                    window.minSegment :: window.minSegment :: List.map (fun _ -> window.minSegment) rest
    
    let toNumber (window: WindowInfo) (segments: uint64 list) =
        let mult = window.maxSegment + 1UL
        List.fold (fun acc next -> acc * mult + next) 0UL segments
        
    let toSegments (window: WindowInfo) (value: uint64) =
        let mult = window.maxSegment + 1UL
        let unfold1 v = if v = 0UL then None else Some(v % mult, v / mult)
        let parts = List.rev (List.unfold unfold1 value)
        
        match parts with
        | [] -> [window.minSegment]
        | f::r when f >= window.minSegment -> parts
        | f::r ->
            parts |> List.map (fun _ -> window.minSegment)
    
    let rec invalidIdsInRange (window: WindowInfo) (min: uint64) (max: uint64) =
        let segments = toSegments window min
        let next = nextInvalidId window segments
        let nextV = toNumber window next
        if nextV <= max then
            nextV :: invalidIdsInRange window (nextV + 1UL) max
        else
            []
        
        
    let isSillyNumber (n: uint64) =
        let chrs = List.ofSeq (string n)
        let windows = [1..List.length chrs]
        let testWindow chrs window =
            let windowed = List.chunkBySize window chrs
            match windowed with
            | []
            | [_] -> false
            | f::r -> List.forall ((=)f)r
        List.exists (testWindow chrs) windows
    let debug_bruteforce minv maxv =
        let vals = [minv..maxv]
        vals |> List.where isSillyNumber
    
        
    let run2 (input: string list) =
        let parsed = input |> List.where ((<>) "") |> List.map parseLine
               
        let mutable sum = 0UL
        for (min, max) in parsed do
            printfn "%i-%i" min max
            let maxWindowSize = log10 max
            let windows = [1..maxWindowSize+1] |> List.map WindowInfo.create
            let invalids =
                windows
                |> List.collect (fun w -> invalidIdsInRange w min max)
                |> List.distinct
                       
            for i in invalids do
                printfn "  -> %i" i
                sum <- sum + i
            
        sum