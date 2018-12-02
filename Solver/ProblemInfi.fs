module ProblemInfi

    open System.Collections.Generic
    open Util

    type Grid = bool[] [,]

    // top left corner is (0,0), bottom right corner is (w-1,h-1)
    let parseGrid w h input = 
        let parseChar c = 
            match c with 
            | '║' -> [| false; true; false; true |] // right; up; left; down
            | '═' -> [| true; false; true; false |]
            | '╔' -> [| true; false; false; true |]
            | '╗' -> [| false; false; true; true |]
            | '╚' -> [| true; true; false; false |]
            | '╝' -> [| false; true; true; false |]
            | '╠' -> [| true; true; false; true |]
            | '╦' -> [| true; false; true; true |]
            | '╩' -> [| true; true; true; false |]
            | '╣' -> [| false; true; true; true |]
            | '╬' -> [| true; true; true; true |]
            | _ -> invalidOp "bad character"
        let sc = Scanner(input, false)
        let grid = Array2D.init w h (fun _ _ -> Array.create 4 false)
        sc.Lines |> Seq.iteri (fun y line -> line |> Seq.iteri (fun x c -> grid.[x,y] <- parseChar c ))
        grid

    let getNeighbours w h (grid: Grid) (x,y) =
        let getDvs() = 
            let dv = [| (1,0); (0,-1); (-1,0); (0,1) |] // right, up, left, down
            grid.[x,y] |> Seq.mapi (fun i v -> (i,v)) |> Seq.filter snd |> Seq.map (fun (i,_) -> (i,dv.[i]))
        let (++) (x,y) (dx,dy) = (x+dx,y+dy)
        getDvs () 
            |> Seq.map (fun (i,dv) -> (i,(x,y) ++ dv))
            |> Seq.filter (fun (_,(nx,ny)) -> 0 <= nx && nx < w && 0 <= ny && ny < h) // take only neignbouring points that fall in grid
            |> Seq.filter (fun (i,(nx,ny)) -> grid.[nx,ny].[(i+2)%4] ) // check that neighbouring point also connects in the right way
            |> Seq.map snd
    
    let solveSilver w h input = 
        let grid = parseGrid w h input
        let bfs (start: int*int) endd = 
            let queue = LinkedList<int*int>()
            let inQueue = HashSet<int*int>() // has ever been in the queue
            let dist = Dictionary<int*int, int>() // distance from start to pos
            queue.AddLast(start) |> ignore
            inQueue.Add(start) |> ignore
            dist.[start] <- 0
            while queue.Count > 0 do
                let curPos = queue.First.Value
                let d = dist.[curPos]
                queue.RemoveFirst()
                for nb in getNeighbours w h grid curPos do
                    if not(inQueue.Contains(nb)) then
                        inQueue.Add(nb) |> ignore
                        queue.AddLast(nb) |> ignore
                        dist.[nb] <- d+1
            dist.[endd]

        bfs (0,0) (w-1,h-1)

    type State = {  mutable nextMoverIsRow: bool;
                    mutable nextMovingRowOrColumnIndex: int; }

    let solveGold w h input = 
        let grid = parseGrid w h input
        let state = {   nextMoverIsRow = true;
                        nextMovingRowOrColumnIndex = 0; }
        let start = (0,0)
        let endd = (w-1,h-1)

        let updateStateAndGrid() = 
            if state.nextMoverIsRow then
                let movingRow = state.nextMovingRowOrColumnIndex%h
                // move row movingRow 1 to the right
                let tmp = grid.[w-1, movingRow]
                for x in w-1..-1..1 do
                    grid.[x, movingRow] <- grid.[x-1, movingRow]
                grid.[0, movingRow] <- tmp
            else
                let movingColumn = state.nextMovingRowOrColumnIndex%w
                // move column movingColumn 1 down
                let tmp = grid.[movingColumn, h-1]
                for y in h-1..-1..1 do  
                    grid.[movingColumn, y] <- grid.[movingColumn, y-1]
                grid.[movingColumn, 0] <- tmp
            state.nextMoverIsRow <- not state.nextMoverIsRow
            state.nextMovingRowOrColumnIndex <- state.nextMovingRowOrColumnIndex + 1        
        
        // maintain: at time t (so after t steps), at which positions can Santa be located?
        let reachablePositions = Dictionary<int, HashSet<int*int>>()
        reachablePositions.[0] <- HashSet( [start] )
        let mutable t = 0
        while not(reachablePositions.[t].Contains(endd)) do 
            reachablePositions.[t+1] <- HashSet()
            for pos in reachablePositions.[t] do
                for nb in getNeighbours w h grid pos do
                    let (nx,ny) = nb
                    let newPos = 
                        // move Santa along, if he's on a moving row/column
                        if nb = endd then
                            (nx,ny)
                        elif state.nextMoverIsRow && ny = (state.nextMovingRowOrColumnIndex%h) then
                            ((nx+1)%w, ny)
                        elif not state.nextMoverIsRow && nx = (state.nextMovingRowOrColumnIndex%w) then
                            (nx, (ny+1)%h)
                        else
                            (nx,ny)
                    reachablePositions.[t+1].Add(newPos) |> ignore

            t <- t+1
            updateStateAndGrid()

        t
