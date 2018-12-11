module Problem3

    open System
    open Util
    open SeqExt

    type Rectangle = {  id: int;
                        offset: int*int;
                        size: int*int; }

    let parseInput input = 
        let parseRectangle line = 
            let sc = Scanner(line, false, " \t\r\n#@,x:")
            { id = sc.NextInt().Value;
              offset = (sc.NextInt().Value, sc.NextInt().Value);
              size   = (sc.NextInt().Value, sc.NextInt().Value)     }
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseRectangle |> Seq.toArray

    let getGrid rectangles = 
        let maxX = rectangles |> Seq.map (fun {id=_;offset=(offsetX,_);size=(w,_)} -> offsetX + w) |> Seq.max
        let maxY = rectangles |> Seq.map (fun {id=_;offset=(_,offsetY);size=(_,h)} -> offsetY + h) |> Seq.max
        let cnt = Array2D.create maxX maxY 0
        for r in rectangles do
            let (ox,oy) = r.offset
            let (w,h) = r.size
            for x in ox..ox+w-1 do
                for y in oy..oy+h-1 do
                    cnt.[x,y] <- cnt.[x,y] + 1
        cnt
    
    let solveSilver input = 
        let rectangles = parseInput input
        let grid = getGrid rectangles
        grid |> Seq.cast<_> |> Seq.filter (fun c -> c >= 2) |> Seq.length

    let solveGold input = 
        let rectangles = parseInput input
        let grid = getGrid rectangles
        let isOk {id=_;offset=(ox,oy);size=(w,h)} = 
            SeqExt.cartesianProduct [ox..ox+w-1] [oy..oy+h-1] |> Seq.forall (fun (x,y) -> grid.[x,y] = 1)
        (rectangles |> Seq.find isOk).id
