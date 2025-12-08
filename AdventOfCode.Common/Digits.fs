namespace AdventOfCode2024.Common

type Digits =
    static member digitsLE (x: uint64) =
        if x = 0UL
        then Seq.singleton 0
        else Seq.unfold (fun x -> if x = 0UL then None else Some (int (x % 10UL), x / 10UL)) x
        
    static member digitsLE (x: int) =
        if x = 0
        then Seq.singleton 0
        else Seq.unfold (fun x -> if x = 0 then None else Some (x % 10, x / 10)) x
        
    static member digitCount (x: uint64) =
        Digits.digitsLE x |> Seq.length
        
    static member digitCount (x: int) =
        Digits.digitsLE x |> Seq.length
        
    static member uint64FromDigitsBE (digits: #seq<int>) =
        Seq.fold (fun v d -> v * 10UL + uint64 d) 0UL digits
