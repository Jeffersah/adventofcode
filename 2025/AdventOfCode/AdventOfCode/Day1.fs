module AdventOfCode.Day1

let parseLine (input: string) =
    if input.StartsWith('L') then
        -(int (input.Substring(1)))
    else
        int (input.Substring(1))

let wrap modulo n =
    ((n % modulo) + modulo) % modulo

let wrapCount modulo n plus=
    let wrapCount modulo n plus =
        let div = (n + plus) / modulo
        let rem = (n + plus) % modulo
        
        abs div, (rem + modulo) % modulo
    if plus < 0 && n = 0 then
        let wc, n = wrapCount modulo (n - modulo) plus
        wc - 1, n
    elif plus < 0 then
        wrapCount modulo (n - modulo) plus
    else
        wrapCount modulo n plus

let run (input: string list) =
    let moves = List.map parseLine input
    let wrap = wrap 100
    
    let dial = List.scan (fun s m -> wrap (s + m)) 50 moves
    
    List.countBy id dial |> List.sortBy fst
    
let run2 (input: string list) =
    let moves = List.map parseLine input
    let wrap = wrapCount 100
    
    let scan1 (dial, zeros) move =
        let (clicks, dial) = wrap dial move
        (dial, zeros + clicks)
    
    let dial = List.scan scan1 (50, 0) moves
    
    // printfn "%A" (List.zip moves dial.Tail)
    
    List.last dial |> snd