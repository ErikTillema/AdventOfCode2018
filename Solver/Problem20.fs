module Problem20

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let getIndexOf = 
        let chars = "NESW".ToCharArray()
        (fun c -> chars |> Array.findIndex ((=) c))
    let dv = [| (0,1); (1,0); (0,-1); (-1,0) |]  // N E S W. North = up = (0,1)
    let (++) (x,y) (dx,dy) = (x+dx,y+dy)

    let parseGrid (input: char[]) = 
        let mutable charIndex = 0
        let mutable pos = (0,0)
        let grid = Dictionary<int*int, bool[]>() // (x,y) -> [| true; false; true; false |] // has door N E S W
        grid.[(0,0)] <- Array.create 4 false
        let move i = 
            grid.[pos].[i] <- true
            pos <- pos ++ dv.[i]
            if not(grid.ContainsKey(pos)) then grid.[pos] <- Array.create 4 false
            grid.[pos].[(i+2)%4] <- true

        let peekChar() = input.[charIndex]

        let readChar() =
            let c = input.[charIndex]
            charIndex <- charIndex + 1
            c
        
        let rec followPath() =
            match peekChar()  with
            | '|' | ')' | '$' -> () // end
            | '(' ->
                readChar() |> ignore // (
                followPaths pos
                readChar() |> ignore // )
                followPath()
            | 'N' | 'E' | 'S' | 'W' -> 
                move (getIndexOf (readChar()))
                followPath()
            | _ -> invalidOp "bad character"
        and followPaths start =
            match peekChar() with
            | '|' -> 
                readChar() |> ignore // |
                pos <- start
                followPaths start
            | ')' -> () // end
            | _ -> 
                followPath()
                followPaths start

        readChar() |> ignore // ^
        followPath()
        readChar() |> ignore // $

        grid

    let getShortestPaths (grid: Dictionary<int*int,bool[]>) start =
        let getNeighbours pos =
            grid.[pos] |> Seq.indexed |> Seq.filter (fun (_,hasDoor) -> hasDoor) |> Seq.map (fun (i,_) -> pos ++ dv.[i])

        // do bfs to get all shortest paths from (0,0)
        let distance = Dictionary<int*int,int>()
        let queue = Queue<int*int>()
        distance.[start] <- 0
        queue.Enqueue(start)
        while queue.Count > 0 do
            let pos = queue.Dequeue()
            let d = distance.[pos]
            for nb in getNeighbours pos do
                if not(distance.ContainsKey(nb)) then
                    distance.[nb] <- d+1
                    queue.Enqueue(nb)

        distance

    let solveSilver (input: string) = 
        let grid = parseGrid (input.ToCharArray())
        let distance = getShortestPaths grid (0,0)
        distance.Values |> Seq.max

    let solveGold (input: string) = 
        let grid = parseGrid (input.ToCharArray())
        let distance = getShortestPaths grid (0,0)
        distance.Values |> Seq.filter (fun d -> d >= 1000) |> Seq.length

