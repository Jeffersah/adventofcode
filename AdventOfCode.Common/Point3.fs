namespace AdventOfCode.Common

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type Point3<'a> = { x: 'a; y: 'a; z: 'a }

module Point3 =
    let create x y z = { x = x; y = y; z = z }
    let toTuple { x = x; y = y; z = z } = (x, y, z)
    let toVTuple { x = x; y = y; z = z } = struct(x, y, z)
    let ofTuple (x, y, z) = create x y z
    let ofVTuple (struct(x, y, z)) = create x y z
    let cardinals = [| create 1 0 0; create 0 1 0; create 0 0 1; create -1 0 0; create 0 -1 0; create 0 0 -1 |]
    let octals =
        let i = [-1..1]
        List.allPairs i (List.allPairs i i)
        |> List.where(fun (a, (b, c)) -> a <> 0 || b <> 0 || c <> 0)
        |> List.map (fun (x, (y, z)) -> create x y z)
        |> Array.ofList
        
    let setX x p : 'a Point3 = { p with x = x }
    let setY y p : 'a Point3 = { p with y = y }
    let setZ z p : 'a Point3  = { p with z = z }
    
    let map fn { x = x; y = y; z = z } = { x = fn x; y = fn y; z = fn z }
    let apply { x = fx; y = fy ; z = fz} { x = x; y = y; z = z } = { x = fx x; y = fy y; z = fz z }
    let ret v =  create v v v
    
    let mapX x p : 'a Point3 = { p with x = x p.x }
    let mapY y p : 'a Point3 = { p with y = y p.y }
    let mapZ z p : 'a Point3  = { p with z = z p.z }
    
    let X { x = x} = x
    let Y { y = y} = y
    let Z { z = z} = z
    
    let inline manhattanLen { x = x; y = y; z = z} = abs x + abs y + abs z
    let inline actualLen { x = x; y = y; z = z} = double x * double x + double y * double y + double z * double z
    
    let all predicate { x = x; y = y; z = z } = predicate x && predicate y && predicate z
    let any predicate { x = x; y = y; z = z } = predicate x || predicate y || predicate z
    let componentwise fn a b = { x = fn a.x b.x; y = fn a.y b.y; z = fn a.z b.z }
        
    let inline negate { x = x; y = y; z = z } = { x = -x; y = -y; z = -z }
    let inline add { x = x; y = y; z = z } { x = x2; y = y2; z = z2 } = { x = x + x2; y = y + y2; z = z + z2 }
    let inline sub { x = x; y = y; z = z } { x = x2; y = y2; z = z2 } = { x = x - x2; y = y - y2; z = z - z2 }
    let inline mul { x = x; y = y; z = z } { x = x2; y = y2; z = z2 } = { x = x * x2; y = y * y2; z = z * z2 }
    let inline div { x = x; y = y; z = z } { x = x2; y = y2; z = z2 } = { x = x / x2; y = y / y2; z = z / z2 }
    let inline dot { x = x; y = y; z = z } { x = x2; y = y2; z = z2 } = x * x2 + y * y2 + z * z2
    
    module Operators =
        let inline (~-) (a: 'a Point3) =
            negate a
        let inline (+) (a: 'a Point3, b: 'a Point3) =
            add a b
        let inline (-) (a: 'a Point3, b: 'a Point3) =
            sub a b
        let inline ( ** ) (a: 'a Point3, b: 'a Point3) =
            mul a b
        let inline ( /* ) (a: 'a Point3, b: 'a Point3) =
            div a b