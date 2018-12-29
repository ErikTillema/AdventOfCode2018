module Problem22

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type RegionType = Rocky | Wet | Narrow
    type Equipment = Torch | ClimbingGear | Neither

    let solveSilver depth target = 
        let (targetX,targetY) = target
        let grid = Array2D.create (targetX+1) (targetY+1) 0 // (x,y) -> erosion level
        for x in 0..targetX do
            for y in 0..targetY do
                let geologicIndex = 
                    match x,y with
                    | 0,0 -> 0
                    | p when p=target -> 0
                    | 0,_ -> y*48271
                    | _,0 -> x*16807
                    | _   -> grid.[x-1,y] * grid.[x,y-1]
                grid.[x,y] <- (geologicIndex + depth)%20183
        let getRiskLevel erosionLevel = erosionLevel % 3
        grid |> Array2D.map getRiskLevel |> Seq.cast<int> |> Seq.sum
        
    let solveGold depth target = 
        // don't use a fixed grid, because we don't know how big our area of search to the shortest path will be.
        // So we use a run-time calculated grid instead.
        let rec getErosionLevel = 
            let geologicIndex (x,y) = 
                match x,y with
                | 0,0 -> 0
                | p when p=target -> 0
                | 0,_ -> y*48271
                | _,0 -> x*16807
                | _   -> getErosionLevel (x-1,y) * getErosionLevel (x,y-1)
            let cache = Dictionary<int*int,int>() // (x,y) -> erosion level
            (fun (x,y) ->   match cache.TryGetValue((x,y)) with
                            | false, _ -> let result = ((geologicIndex (x,y)) + depth)%20183
                                          cache.[(x,y)] <- result
                                          result
                            | true, v  -> v
            )

        let getRegionType (x,y) = 
            match (getErosionLevel (x,y))%3 with
            | 0 -> Rocky
            | 1 -> Wet
            | _ -> Narrow
        let canEnter equipment pos =
            match getRegionType pos, equipment with
            | Rocky, Torch -> true
            | Rocky, ClimbingGear -> true
            | Wet, ClimbingGear -> true
            | Wet, Neither -> true
            | Narrow, Torch -> true
            | Narrow, Neither -> true
            | _ -> false

        let dv = [| (1,0); (-1,0); (0,1); (0,-1) |]
        let allEquipments = [ Torch; ClimbingGear; Neither ] 
        let (++) (x,y) (dx,dy) = (x+dx,y+dy)
        let isInBounds (x,y) = x >= 0 && y >= 0
        let getNeighbours (pos, equipment) = 
            seq {
                yield! dv   |> Seq.map ((++) pos) 
                            |> Seq.filter isInBounds 
                            |> Seq.filter (canEnter equipment)
                            |> Seq.map (fun newPos -> (1,(newPos,equipment)))
                yield! allEquipments
                            |> Seq.filter ((<>) equipment)
                            |> Seq.filter (fun newEquipment -> canEnter newEquipment pos)
                            |> Seq.map (fun newEquipment -> (7,(pos,newEquipment)))
            }

        // A* algorithm
        let getDistance (x1,y1) (x2,y2) = abs(x2-x1) + abs(y2-y1) // use as heuristic lower bound
        let q = SortedSet<int*((int*int)*Equipment)>() // f-score, ((x,y), Equipment)
        let gScore = Dictionary<(int*int)*Equipment, int>() // g-score: distance from start to node
        let fScore = Dictionary<(int*int)*Equipment, int>() // f-score: distance from start to node + lower bound for remaining distance from node to endd
        let start = ((0,0),Torch)
        let endd = (target,Torch)
        gScore.[start] <- 0
        fScore.[start] <- getDistance (0,0) target
        q.Add((fScore.[start], start)) |> ignore
        let mutable continu = true
        while q.Count > 0 && continu do 
            let _,cur = q.Min
            q.Remove(q.Min) |> ignore
            let g = gScore.[cur]
            if cur = endd then continu <- false
            for (dEdge,nb) in getNeighbours cur do
                let (nbPos,_) = nb
                let trialg = g + dEdge
                let trialf = g + dEdge + getDistance nbPos target
                match gScore.TryGetValue nb with
                | false, _       -> 
                    q.Add((trialf,nb)) |> ignore
                    gScore.[nb] <- trialg
                    fScore.[nb] <- trialf
                | true, ng when trialg < ng  -> // relax
                    let nf = fScore.[nb]
                    q.Remove((nf,nb)) |> ignore
                    q.Add((trialf,nb)) |> ignore
                    gScore.[nb] <- trialg
                    fScore.[nb] <- trialf
                | _ -> ()

        gScore.[endd]