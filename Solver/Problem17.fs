module Problem17

    open System
    open Util
    open SeqExt
    open System.Collections.Generic
    open System

    type GridPos = Free | Wall
    type Water = Flowing | HalfStill | Still | Undetermined

    let parseGrid input = 
        let sc = Scanner(input, false)
        let parseLine line =
            match line with
            | Regex "^x=(?<x>\d+), y=(?<y1>\d+)..(?<y2>\d+)$" [ x; y1; y2] -> ((int x, int x),(int y1, int y2))
            | Regex "^y=(?<y>\d+), x=(?<x1>\d+)..(?<x2>\d+)$" [ y; x1; x2] -> ((int x1, int x2),(int y, int y))
            | _ -> invalidOp "bad line"
        let lines = sc.Lines |> Seq.map parseLine |> Seq.toArray
        let minX = (lines |> Seq.map (fun ((x1,x2),(_,_)) -> min x1 x2) |> Seq.min) - 1
        let maxX = (lines |> Seq.map (fun ((x1,x2),(_,_)) -> max x1 x2) |> Seq.max) + 1
        let minY = (lines |> Seq.map (fun ((_,_),(y1,y2)) -> min y1 y2) |> Seq.min) - 1
        let maxY = (lines |> Seq.map (fun ((_,_),(y1,y2)) -> max y1 y2) |> Seq.max)
        let w = maxX - minX + 1
        let h = maxY - minY + 1
        let grid = Array2D.create w h Free
        for ((x1,x2),(y1,y2)) in lines do
            for x in x1..x2 do
                for y in y1..y2 do
                    grid.[x-minX,y-minY] <- Wall
        (grid, w, h, minX, minY)

    let getFlow (grid: GridPos[,]) w h minX minY = 
        let maxX, maxY = minX+w-1, minY+h-1
        let flow = Dictionary<int*int,Water>()

        let (=*=) (x,y) gridPos = grid.[x-minX,y-minY] = gridPos
        let (=~=) (x,y) water = flow.[(x,y)] = water
        let (<~-) (x,y) water = flow.[(x,y)] <- water

        let rec setStill (x,y) = // mark water left and right as Still
            if (x+1,y) =*= Free && (x+1,y) =~= HalfStill then 
                (x+1,y) <~- Still
                setStill (x+1,y)
            if (x-1,y) =*= Free && (x-1,y) =~= HalfStill then 
                (x-1,y) <~- Still
                setStill (x-1,y)

        let rec dfs (x,y) canGoLeft canGoRight =
            if flow.ContainsKey(x,y) then
                flow.[(x,y)]
            else
                let result = 
                    if y+1 > maxY then 
                        Flowing
                    elif (x,y+1) =*= Wall || (dfs (x,y+1) true true) = Still then
                        // wall or still water below
                        // start flowing left and right
                        let left  = 
                            if (x-1,y) =*= Wall then Still
                            elif not canGoLeft then Undetermined
                            else dfs (x-1,y) true false
                        let right = 
                            if (x+1,y) =*= Wall then Still
                            elif not canGoRight then Undetermined 
                            else dfs (x+1,y) false true
                    
                        match left,right with
                        | Still,     Undetermined 
                        | HalfStill, Undetermined 
                        | Undetermined, Still     
                        | Undetermined, HalfStill -> HalfStill
                        | HalfStill, HalfStill    
                        | Still    , HalfStill    
                        | HalfStill, Still        
                        | Still    , Still        -> Still
                        | _                       -> Flowing
                    else
                        // no wall and no still water below, so keeps flowing forever
                        Flowing 
                (x,y) <~- result
                if result = Still then setStill (x,y) // if water is still, also water to left and right must be still.
                result

        dfs (500,minY) true true |> ignore
        flow

        //for y in minY..maxY do
        //    for x in minX..maxX do
        //        if flow.ContainsKey((x,y)) then 
        //            if flow.[x,y] = Flowing then printf "|"
        //            elif flow.[x,y] = Still then printf "~"
        //            elif flow.[x,y] = HalfStill then printf "x"
        //        elif grid.[x-minX,y-minY] = Wall then printf "#"
        //        else printf "."
        //    printfn ""

    let solveSilver input = 
        let (grid, w, h, minX, minY) = parseGrid input
        let flow = getFlow grid w h minX minY
        flow |> Seq.filter (fun kvp -> snd kvp.Key >= minY+1) |> Seq.length

    let solveGold input = 
        let (grid, w, h, minX, minY) = parseGrid input
        let flow = getFlow grid w h minX minY
        flow 
            |> Seq.filter (fun kvp -> snd kvp.Key >= minY+1) 
            |> Seq.filter (fun kvp -> kvp.Value = Still)
            |> Seq.length
