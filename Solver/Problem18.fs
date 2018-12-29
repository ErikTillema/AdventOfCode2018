module Problem18

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type GridPos = Open | Trees | Lumberyard

    let parseGrid w h input = 
        let grid = Array2D.create w h Open
        let sc = Scanner(input,false)
        sc.Lines |> Seq.iteri (fun y line ->
            line |> Seq.iteri (fun x c ->
                match c with
                | '.' -> grid.[x,y] <- Open
                | '|' -> grid.[x,y] <- Trees
                | '#' -> grid.[x,y] <- Lumberyard
                | _ -> invalidOp "bad character"
            )
        )
        grid

    let evolve w h (grid: GridPos[,]) = 
        let dv = [| (1,1); (1,0); (1,-1); (0,1); (0,-1); (-1,1); (-1,0); (-1,-1) |]
        let (++) (x,y) (dx,dy) = (x+dx,y+dy)
        let inbounds (x,y) = 0 <= x && x < w && 0 <= y && y < h
        let count gridPos pos =
            dv |> Seq.map ((++) pos) |> Seq.filter inbounds |> Seq.filter (fun (x,y) -> grid.[x,y] = gridPos) |> Seq.length
            
        let newGrid = Array2D.create w h Open
        for x in 0..w-1 do
            for y in 0..h-1 do
                newGrid.[x,y] <- 
                    match grid.[x,y] with
                    | Open ->
                        if count Trees (x,y) >= 3 then Trees 
                        else Open
                    | Trees ->
                        if count Lumberyard (x,y) >= 3 then Lumberyard
                        else Trees
                    | Lumberyard ->
                        if count Trees (x,y) >= 1 && count Lumberyard (x,y) >= 1 then Lumberyard
                        else Open
        
        for x in 0..w-1 do
            for y in 0..h-1 do
                grid.[x,y] <- newGrid.[x,y]

    let getResult grid = 
        let treeCount = grid |> Seq.cast<_> |> Seq.filter ((=) Trees) |> Seq.length
        let lumberyardCount = grid |> Seq.cast<_> |> Seq.filter ((=) Lumberyard) |> Seq.length
        treeCount * lumberyardCount

    let solveSilver w h input = 
        let grid = parseGrid w h input
        for _ in 1..10 do evolve w h grid
        getResult grid
    
    let solveGold w h input = 
        let grid = parseGrid w h input
        let getHash() = // we hope that this hash will uniquely enough define a grid to differentiate between all encountered grids.
            let getIndex gridPos = 
                match gridPos with 
                | Open -> 0L
                | Trees -> 1L
                | Lumberyard -> 2L
            let mutable hash = 0L
            for x in 0..w-1 do
                for y in 0..h-1 do
                    hash <- 3L*hash + getIndex grid.[x,y]
            hash

        // finds a loop in grid evolutions        
        let rec findLoop map evolutionCount = // map: hash -> evolutionCount
            let hash = getHash()
            match map |> Map.tryFind hash with
            | None ->
                evolve w h grid
                findLoop (map |> Map.add hash evolutionCount) (evolutionCount+1)
            | Some(startOfLoop) ->
                (Map.count map, startOfLoop)

        let (endOfLoop,startOfLoop) = findLoop Map.empty 0
        let lengthOfLoop = endOfLoop - startOfLoop
        let effectiveEvolutionCount = ((1_000_000_000 - startOfLoop) % lengthOfLoop) + startOfLoop 

        let grid = parseGrid w h input
        for _ in 1..effectiveEvolutionCount do evolve w h grid
        getResult grid


