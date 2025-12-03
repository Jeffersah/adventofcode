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