namespace AdventOfCode.Common

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type Point<'a> = { x: 'a; y: 'a }

module Point =
    let create x y = { x = x; y = y }
    let toTuple { x = x; y = y } = (x, y)
    let toVTuple { x = x; y = y } = struct(x, y)
    let ofTuple (x, y) = create x y
    let ofVTuple (struct(x, y)) = create x y
    let cardinals = [| create 1 0; create 0 1; create -1 0; create 0 -1 |]
    let octals =
        let i = [-1..1]
        List.allPairs i i
        |> List.where(fun (a, b) -> a <> 0 || b <> 0)
        |> List.map ofTuple
        |> Array.ofList
        
    let setX x { y = y } = { x = x; y = y }
    let setY y { x = x } = { x = x; y = y }
    let map fn { x = x; y = y } = { x = fn x; y = fn y }
    let apply { x = fx; y = fy } { x = x; y = y } = { x = fx x; y = fy y }
    let ret v =  create v v
    let mapX fn { x = x; y = y } = { x = fn x; y = y }
    let mapY fn { x = x; y = y } = { x = x; y = fn y }
    let X { x = x } = x
    let Y { y = y } = y
    
    let inline manhattanLen { x = x; y = y } = abs x + abs y
    let inline actualLen { x = x; y = y } = double x * double x + double y * double y
    
    let both predicate { x = x; y = y } = predicate x && predicate y
    let either predicate { x = x; y = y } = predicate x || predicate y
    let componentwise fn a b = { x = fn a.x b.x; y = fn a.y b.y }
        
    let inline negate { x = x; y = y } = { x = -x; y = -y }
    let inline add { x = x; y = y } { x = x2; y = y2 } = { x = x + x2; y = y + y2 }
    let inline sub { x = x; y = y } { x = x2; y = y2 } = { x = x - x2; y = y - y2 }
    let inline mul { x = x; y = y } { x = x2; y = y2 } = { x = x * x2; y = y * y2 }
    let inline div { x = x; y = y } { x = x2; y = y2 } = { x = x / x2; y = y / y2 }
    let inline dot { x = x; y = y } { x = x2; y = y2 } = x * x2 + y * y2
    
    module Operators =
        let inline (~-) (a: 'a Point) = { x = -a.x; y = -a.y }
        let inline (+) (a: 'a Point, b: 'a Point) =
            { x = a.x + b.x; y = a.y + b.y }
        let inline (-) (a: 'a Point, b: 'a Point) =
            { x = a.x - b.x; y = a.y - b.y }
        let inline ( ** ) (a: 'a Point, b: 'a Point) =
            { x = a.x * b.x; y = a.y * b.y }
        let inline ( /* ) (a: 'a Point, b: 'a Point) =
            { x = a.x / b.x; y = a.y / b.y }