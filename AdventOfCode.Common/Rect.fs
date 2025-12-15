namespace AdventOfCode.Common

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type Rect<'a> = { x: 'a; y: 'a; w: 'a; h: 'a  }

module Rect =
    let create x y w h = { x = x; y = y; w = w; h = h }
    let inline ofBounds topLeft bottomRight =
        let size = Point.sub bottomRight topLeft |> Point.map abs |> Point.add (Point.create (GenericMath.ofInt 1) (GenericMath.ofInt 1))
        let tl = Point.componentwise min topLeft bottomRight
        { x = tl.x; y = tl.y; w = size.x; h = size.y }
    
    let xaxis { x = x; w = w } = Range.create x w
    let yaxis { y = y; h = h } = Range.create y h
    
    let inline area { w = w; h = h } = w * h
    let inline perimeter { w = w; h = h } = 2 * (w + h)
    
    let inline contains { x = x; y = y; w = w; h = h } (pt: 'a Point) =
        pt.x >= x && pt.x < x + w && pt.y >= y && pt.y < y + h
        
    let inline intersection a b =
        let ax, ay = xaxis a, yaxis a
        let bx, by = xaxis b, yaxis b
        Range.intersection ax bx
        |> Option.bind (fun xr ->
                Range.intersection ay by
                |> Option.map (fun yr ->
                    create xr.start yr.start xr.length yr.length
                )
        )
        
    