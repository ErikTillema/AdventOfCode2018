module Problem11

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let w = 300

    let getValue serial (x,y) = 
        ((((x+1+10)*(y+1) + serial)*(x+1+10)/100)%10)-5

    let solveSilver serial = 
        let grid = Array2D.init w w (fun x y -> getValue serial (x,y))
        let size = 3
        let getSummedValue (x,y) =
            grid.[x  ,y] + grid.[x  ,y+1] + grid.[x  ,y+2] + 
            grid.[x+1,y] + grid.[x+1,y+1] + grid.[x+1,y+2] + 
            grid.[x+2,y] + grid.[x+2,y+1] + grid.[x+2,y+2]
            // is faster than SeqExt.cartesianProduct [0..size-1] [0..size-1] |> Seq.sumBy (fun (dx,dy) -> grid.[x+dx,y+dy])
        let (x,y) = SeqExt.cartesianProduct [0..w-size] [0..w-size] |> Seq.maxBy getSummedValue
        (x+1,y+1)

    let solveGold serial = 
        let grid = Array2D.init w w (fun x y -> getValue serial (x,y))
        let horizontalSum = Array3D.create (w+1) w w Int32.MinValue // sum of (x,y) until (x+s,y)
        let verticalSum = Array3D.create (w+1) w w Int32.MinValue   // sum of (x,y) until (x,y+s)
        let sum = Array3D.create (w+1) w w Int32.MinValue           // sum of (x,y) until (x+s,y+s)
        for x in 0..w-1 do
            for y in 0..w-1 do
                horizontalSum.[1,x,y] <- grid.[x,y]
                verticalSum.[1,x,y] <- grid.[x,y]
                sum.[1,x,y] <- grid.[x,y]
        for s in 2..w do
            for x in 0..w-s do
                for y in 0..w-1 do
                    horizontalSum.[s,x,y] <- horizontalSum.[s-1,x,y] + grid.[x+s-1,y]
        for s in 2..w do
            for x in 0..w-1 do
                for y in 0..w-s do
                    verticalSum.[s,x,y] <- verticalSum.[s-1,x,y] + grid.[x,y+s-1]
        for s in 2..w do
            for x in 0..w-s do
                for y in 0..w-s do
                    sum.[s,x,y] <- sum.[s-1,x,y] + horizontalSum.[s,x,y+s-1] + verticalSum.[s-1,x+s-1,y]

        //sum |> Array3D.mapi ... |> Seq.maxBy ... is very slow!
        //SeqExt.cartesianProduct (SeqExt.cartesianProduct [0..w-1] [0..w-1]) [1..w] |> Seq.maxBy (fun ((x,y),s) -> sum.[s,x,y]) // still 3 times slower.
        // the following is not very elegant, but the fastest:
        let mutable best = (0,0,0)
        let mutable bestSum = Int32.MinValue
        let updateBest s x y sum = 
            if sum > bestSum then 
                bestSum <- sum
                best <- (s,x,y)
        sum |> Array3D.iteri updateBest
        let (bestS,bestX,bestY) = best
        (bestX+1,bestY+1,bestS)

