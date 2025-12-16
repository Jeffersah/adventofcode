module AdventOfCode.Day10

open System
open System.Drawing
open System.Net.Http.Headers
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open System.Xml.Linq
open AdventOfCode
open AdventOfCode.Common

type Problem = { lights: uint64; buttons: uint64 list; joltages: uint64 list }

let parseInput (input: string list)=
    let regex =
        System.Text.RegularExpressions.Regex("\\[(?'lights'[.#]+)\\]( \\((?'button'\\d+(,\\d+)*)\\))+ {(?'joltages'\d+(,\d+)*)}", System.Text.RegularExpressions.RegexOptions.Compiled)
    let parse1 (line: string) =
        let matched = regex.Match(line)
        let lightPart = matched.Groups["lights"].Value
        let buttonParts = matched.Groups["button"].Captures |> Seq.map _.Value
        let joltagePart = matched.Groups["joltages"].Value
        
        let indexToUint (value: int) : uint64 =
            1UL <<< value
            
        let rec lightsToUInt lights acc =
            match lights with
            | '.'::xs -> lightsToUInt xs (acc <<< 1)
            | '#'::xs -> lightsToUInt xs ((acc <<< 1) ||| 1UL)
            | [] -> acc
            
        let lightsUint = lightsToUInt (List.rev <| List.ofSeq lightPart) 0UL
        
        let buttonToUint (button: string) =
            let spl = button.Split(',')
            spl
            |> Seq.map int
            |> Seq.map indexToUint
            |> Seq.fold (|||) 0UL
            
        let joltages =
            joltagePart.Split(',')
            |> Seq.map uint64
            |> List.ofSeq
        
        let buttons = buttonParts |> Seq.map buttonToUint |> List.ofSeq
        { lights = lightsUint; buttons = buttons; joltages = joltages }
        
    List.map parse1 input
        
let rec solve (lights: uint64) (buttons: uint64 list) (pressCount: int) =
    match lights, buttons with
    | 0UL, _ -> ValueSome pressCount
    | _, [] -> ValueNone
    | lights, b::bs ->
        let buttonUnion = List.fold (|||) 0UL buttons
        let noButton = ~~~ buttonUnion
        if lights &&& noButton <> 0UL then
            ValueNone
        else
            let noPress = solve lights bs pressCount
            let press = solve (lights ^^^ b) bs (pressCount + 1)
            match noPress, press with
            | ValueNone, x
            | x, ValueNone -> x
            | ValueSome a, ValueSome b -> ValueSome (min a b)


let run (input: string list) =
    let problems = parseInput input
    let solutions =
        problems
        |> List.map (fun problem -> solve problem.lights problem.buttons 0)
    solutions |> List.sumBy (_.Value)

let rec packedButtonToList (button: uint64) =
    match button with
    | 0UL -> []
    | x when x % 2UL = 1UL ->
        true :: packedButtonToList (button >>> 1)
    | _ ->
        false :: packedButtonToList (button >>> 1)

let maxPresses joltages button =
    

let run2 (input: string list) =
    let problems = parseInput input
    for problem in problems do
        printfn "%A" problem.joltages
        let buttons = List.map packedButtonToList problem.buttons
        printfn "%A -> %A" buttons problem.buttons
    ()