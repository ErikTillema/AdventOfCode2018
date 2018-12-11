module Problem6

    open System
    open Util
    open SeqExt
    open System.Collections.Generic
    
    let parseInput input = 
        let parseLine line = 
            match line with
            | Regex "^(?<x>\d+), (?<y>\d+)$" [ x; y ] -> (int x, int y)
            | _ -> invalidOp "bad line"
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseLine |> Seq.toArray

    let getNeighbours w h (x,y) =
        let dv = [| (1,0); (0,1); (-1,0); (0,-1) |]
        let (++) (x,y) (dx,dy) = (x+dx,y+dy)
        dv |> Seq.map ((++) (x,y)) |> Seq.filter (fun (x,y) -> 0 <= x && x < w && 0 <= y && y < h)

    let solveSilver input = 
        let points = parseInput input
        let w = 1 + (points |> Seq.map fst |> Seq.max)
        let h = 1 + (points |> Seq.map snd |> Seq.max)

        let grid = Array2D.create w h (-1,-1) // index, distance
        let queue = LinkedList<int*int>()
        points |> Array.iteri (fun i (x,y) -> grid.[x,y] <- (i, 0))
        points |> Array.iter (fun p -> queue.AddLast(p) |> ignore)
        while queue.Count > 0 do
            let (x,y) = queue.First.Value
            queue.RemoveFirst()
            let index = fst grid.[x,y]
            let d = snd grid.[x,y]
            for (nx,ny) in getNeighbours w h (x,y) do
                let (ni, nd) = grid.[nx,ny]
                if ni = -1 then
                    grid.[nx,ny] <- (index, d+1)
                    queue.AddLast((nx,ny)) |> ignore
                elif nd = d+1 && ni >= 0 && ni <> index then
                    grid.[nx,ny] <- (-2, nd) // '.'
                else 
                    // ignore
                    ()
        
        let cnt = Array.create (Array.length points) 0
        grid |> Seq.cast<int*int> |> Seq.map fst |> Seq.filter (fun i -> i >= 0) |> Seq.iter (fun i -> cnt.[i] <- cnt.[i] + 1)
        let edges = 
            seq {
                yield! [0..w-1] |> Seq.map (fun x -> (x,0))
                yield! [0..w-1] |> Seq.map (fun x -> (x,h-1))
                yield! [0..h-1] |> Seq.map (fun y -> (0,y))
                yield! [0..h-1] |> Seq.map (fun y -> (w-1,y))
            }
        let indicesOnEdge = edges |> Seq.map (fun (x,y) -> grid.[x,y] |> fst) |> Set.ofSeq
        cnt |> Seq.indexed |> Seq.filter (fun (i, _) -> not(indicesOnEdge.Contains(i))) |> Seq.map snd |> Seq.max

    let solveGold distTreshold input = 
        let points = parseInput input
        let n = points |> Array.length
        let w = 1 + (points |> Seq.map fst |> Seq.max)
        let h = 1 + (points |> Seq.map snd |> Seq.max)
        // assume that all requested points are within the grid of w x h
        let cntX = Array.create w 0 // number of points with x=X
        let cntY = Array.create h 0 // number of points with y=Y
        points |> Seq.iter (fun (x,_) -> cntX.[x] <- cntX.[x] + 1)
        points |> Seq.iter (fun (_,y) -> cntY.[y] <- cntY.[y] + 1)
        let cumX = Array.create w 0 // number of points with x<=X
        let cumY = Array.create h 0 // number of points with y<=Y
        cumX.[0] <- cntX.[0]
        cumY.[0] <- cntY.[0]
        for x in 1..w-1 do cumX.[x] <- cumX.[x-1] + cntX.[x]
        for y in 1..h-1 do cumY.[y] <- cumY.[y-1] + cntY.[y]

        let sumDist = Array2D.create w h 0
        for y in 0..h-1 do
            if y = 0 then
                sumDist.[0,0] <- points |> Seq.map (fun (x,y) -> abs(x) + abs(y)) |> Seq.sum
            else 
                sumDist.[0,y] <- sumDist.[0,y-1] + 2*(cumY.[y-1]) - n
            for x in 1..w-1 do 
                sumDist.[x,y] <- sumDist.[x-1,y] + 2*(cumX.[x-1]) - n

        sumDist |> Seq.cast<_> |> Seq.filter (fun d -> d < distTreshold) |> Seq.length
