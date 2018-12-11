module Problem10

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let parseInput input = 
        let parseLine line = 
            match line with
            | Regex "^position=< *(?<px>[-\d]+), *(?<py>[-\d]+)> velocity=< *(?<vx>[-\d]+), *(?<vy>[-\d]+)>$" [ px; py; vx; vy ] -> 
                (int px, int py), (int vx, int vy)
            | _ -> invalidOp "bad line"
        Scanner(input, false).Lines |> Seq.map parseLine |> Seq.toArray

    let (++) (x,y) (dx,dy) = (x+dx,y+dy)
    let (--) (x,y) (dx,dy) = (x-dx,y-dy)
    let ( **) a (dx,dy) = (a*dx,a*dy)
    let length (x,y) = sqrt ((float x)*(float x) + (float y)*(float y))

    let getPositionAt t point =
        let start = point |> fst
        let dx = point |> snd
        start ++ (t ** dx)

    /// returns the time when point1 and point2 are closest to each other
    let findNearestTime point1 point2 = 
        let getDistanceAt t = 
            (getPositionAt t point1) -- (getPositionAt t point2) |> length
        let deltaDist t = 
            getDistanceAt (t+1) - (getDistanceAt t)
        let rec binarySearch start endd =
            if start = endd then start
            else 
                let mid = (start+endd)/2
                let dd = deltaDist mid
                if dd = 0. then mid
                elif dd < 0. then binarySearch (mid+1) endd // distance is decreasing
                else binarySearch start mid // distance is increasing
        binarySearch 0 100_000

    let printAt t points = 
        let positions = points |> Seq.map (getPositionAt t) |> Set.ofSeq
        let minX = positions |> Seq.map fst |> Seq.min
        let minY = positions |> Seq.map snd |> Seq.min
        let maxX = positions |> Seq.map fst |> Seq.max
        let maxY = positions |> Seq.map snd |> Seq.max
        for y in minY..maxY do
            for x in minX..maxX do
                if positions.Contains((x,y)) then printf "#"
                else printf "."
            printfn ""

    let solveGold input = 
        let points = parseInput input
        let n = points.Length
        let count = Dictionary<int,int>() // which nearest time is the most common?
        let rng = new Random()
        for _ in 1..200 do
            let i = rng.Next(0,n)
            let j = rng.Next(0,n)
            let t = findNearestTime points.[i] points.[j]
            if not(count.ContainsKey(t)) then count.[t] <- 0
            count.[t] <- count.[t] + 1
        let t = (count |> Seq.maxBy (fun kvp -> kvp.Value)).Key // most common nearest time
        printAt t points
        t
