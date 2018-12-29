module Problem25

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Point = int*int*int*int
    
    type Node = { point: Point;
                  neighbours: List<Node>; }

    let parsePoints input = 
        let parsePoint line = 
            match line with
            | Regex "^(?<x>[-\d]+),(?<y>[-\d]+),(?<z>[-\d]+),(?<t>[-\d]+)$" [ x; y; z; t ] -> (int x, int y, int z, int t)
            | _ -> invalidOp "bad line"
        Scanner(input, false).Lines |> Seq.map parsePoint |> Seq.sort |> Seq.toArray

    let getNodes (points: Point[]) = 
        let getXDistance (x1,_,_,_) (x2,_,_,_) = 
            abs(x2-x1)
        let getDistance (x1,y1,z1,t1) (x2,y2,z2,t2) = 
            abs(x2-x1) + abs(y2-y1) + abs(z2-z1) + abs(t2-t1)
        let n = points.Length
        let nodes = points |> Array.map (fun p -> { point = p; neighbours = List<Node>(); })
        for i in 0..n-1 do
            [ i+1..n-1 ] |> Seq.takeWhile (fun j -> getXDistance points.[j] points.[i] <= 3) 
                         |> Seq.filter (fun j -> getDistance points.[j] points.[i] <= 3)
                         |> Seq.iter (fun j -> nodes.[i].neighbours.Add(nodes.[j]) |> ignore
                                               nodes.[j].neighbours.Add(nodes.[i]) |> ignore )
        nodes

    let getConnectedComponents nodes = 
        let mutable connectedComponentCount = 0
        let connectedComponent = Dictionary<Node,int>() // Node -> connected component index
        let rec dfs node connectedComponentIndex =
            connectedComponent.[node] <- connectedComponentIndex
            for nb in node.neighbours do
                if not(connectedComponent.ContainsKey(nb)) then
                    dfs nb connectedComponentIndex

        for node in nodes do
            if not(connectedComponent.ContainsKey(node)) then
                dfs node connectedComponentCount
                connectedComponentCount <- connectedComponentCount + 1

        connectedComponentCount

    let solveSilver input = 
        let points = parsePoints input
        let nodes = getNodes points
        getConnectedComponents nodes
        
    