module Problem23

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Position = int*int*int // (x,y,z)
    type Dimensions = int*int*int // (w,l,h)
    type Bot = Bot of Position*int
    type Block = Block of Position*Dimensions // offset and dimensions

    let parseBots input =
        let parseBot line =
            match line with
            | Regex "^pos=\<(?<x>[-\d]+),(?<y>[-\d]+),(?<z>[-\d]+)\>, r=(?<r>\d+)$" [ x; y; z; r ] -> Bot((int x,int y,int z),int r)
            | _ -> invalidOp "bad line"
        Scanner(input, false).Lines |> Seq.map parseBot |> Seq.toArray

    let (++) (x,y,z) (dx,dy,dz) = (x+dx,y+dy,z+dz)

    let getDistance (x1,y1,z1) (x2,y2,z2) =
        abs(x1-x2) + abs(y1-y2) + abs(z1-z2)

    let isInRange (Bot(botPos,r)) pos = 
        getDistance pos botPos <= r

    let solveSilver input = 
        let bots = parseBots input
        let maxRangeBot = bots |> Seq.maxBy (fun (Bot(_,r)) -> r)
        bots |> Seq.filter (fun (Bot(pos,_)) -> isInRange maxRangeBot pos) |> Seq.length

    // Successful attempt: deterministic partitioning strategy
    // Partition the search space into blocks.
    // For each block, calculate a lower bound (min) and upper bound (max) for the number of bots in range of any point within the block.
    // Maintain highest min value of the blocks
    // Remove blocks that can't contain the optimal point (because max < highestmin)
    // Remove blocks that have min==max (and remember the block with the highest min and closest to zero)
    // Partition block into sub-blocks if its min != max
    // start with most promising block (highest max) 
    // continue this process until all blocks are gone
    let solveGold input = 
        let bots = parseBots input
        let minX = bots |> Seq.map (fun (Bot((x,y,z),_)) -> x) |> Seq.min
        let maxX = bots |> Seq.map (fun (Bot((x,y,z),_)) -> x) |> Seq.max
        let minY = bots |> Seq.map (fun (Bot((x,y,z),_)) -> y) |> Seq.min
        let maxY = bots |> Seq.map (fun (Bot((x,y,z),_)) -> y) |> Seq.max
        let minZ = bots |> Seq.map (fun (Bot((x,y,z),_)) -> z) |> Seq.min
        let maxZ = bots |> Seq.map (fun (Bot((x,y,z),_)) -> z) |> Seq.max
        
        let partition block = 
            let partitions = 4
            let (Block((x,y,z),(w,l,h))) = block
            let xPartitions = min partitions w
            let yPartitions = min partitions l
            let zPartitions = min partitions h
            let xs = [|0..xPartitions|] |> Array.map (fun i -> x + (i*w)/xPartitions)
            let ys = [|0..yPartitions|] |> Array.map (fun i -> y + (i*l)/yPartitions)
            let zs = [|0..zPartitions|] |> Array.map (fun i -> z + (i*h)/zPartitions)
            seq {
                for ix in 0..xPartitions-1 do
                    for iy in 0..yPartitions-1 do
                        for iz in 0..zPartitions-1 do
                            yield Block((xs.[ix],ys.[iy],zs.[iz]), (xs.[ix+1]-xs.[ix],ys.[iy+1]-ys.[iy],zs.[iz+1]-zs.[iz]))
            }

        let getMinDistanceFromPoint (xPoint,yPoint,zPoint) (Block((x,y,z),(w,l,h))) = 
            let dx = if x <= xPoint && xPoint <= x+w-1 then 0 else min (abs (x-xPoint)) (abs (x+w-1-xPoint))
            let dy = if y <= yPoint && yPoint <= y+l-1 then 0 else min (abs (y-yPoint)) (abs (y+l-1-yPoint))
            let dz = if z <= zPoint && zPoint <= z+h-1 then 0 else min (abs (z-zPoint)) (abs (z+h-1-zPoint))
            dx + dy + dz

        let getMinDistanceFromZero block = getMinDistanceFromPoint (0,0,0) block

        let getMaxDistanceFromPoint (xPoint,yPoint,zPoint) (Block((x,y,z),(w,l,h))) = 
            let dx = max (abs (x-xPoint)) (abs (x+w-1-xPoint))
            let dy = max (abs (y-yPoint)) (abs (y+l-1-yPoint))
            let dz = max (abs (z-zPoint)) (abs (z+h-1-zPoint))
            dx + dy + dz

        // returns an upper bound for the number of bots in range of any point within the block.
        let getMax block = 
            let counts (Bot(pos,r)) = getMinDistanceFromPoint pos block <= r
            bots |> Seq.filter counts |> Seq.length

        // returns a lower bound for the number of bots in range of any point within the block.
        let getMin block = 
            let counts (Bot(pos,r)) = getMaxDistanceFromPoint pos block <= r
            bots |> Seq.filter counts |> Seq.length

        let mutable highestMin = -1
        let mutable minDistanceFromZero = -1
        let queue = SortedSet<int*Block>() // priority queue of blocks: max, block
        let initialBlock = Block((minX,minY,minZ), (maxX-minX+1,maxY-minY+1,maxZ-minZ+1))
        queue.Add((getMax initialBlock,initialBlock)) |> ignore
        while queue.Count > 0 do
            let (max,curBlock) = queue.Max
            queue.Remove(queue.Max) |> ignore
            if max < highestMin then 
                () // ignore unfinished block
            else
                let min = getMin curBlock
                if min = max then // min==max, finished block.
                    if min > highestMin || (min = highestMin && getMinDistanceFromZero curBlock < minDistanceFromZero) then 
                        highestMin <- min
                        minDistanceFromZero <- getMinDistanceFromZero curBlock
                    else 
                        () // ignore finished block
                else // partition unfinished block.
                    if min > highestMin then highestMin <- min
                    for block in partition curBlock do
                        queue.Add(getMax block, block) |> ignore
        
        minDistanceFromZero
        
    // Failed attempt #1: 
    // Create a graph of bots with an edge between bots if they have overlapping range.
    // Idea being that if 3 bots all mutually have overlapping range, then they must have overall overlapping range.
    // Then we could find the largest clique in the graph.
    // This is hard problem to solve, but I was hoping that the graph would be sparse.
    // However, it is far from sparse: many bots have more than 900 overlapping bots.
    // Since the program would probably never finish, I didn't follow through on this idea.

    // Successful but unsatisfactory attempt #2:
    // Perform a partitioning strategy:
    // Partition the search space into regions. For each region, select some point as region representative.
    // For the representing point, calculate the number of bots in range.
    // Take the region with the highest result and continue with that one. Discard all other regions.
    // Then partition the region with highest result again. Continue until regions have size 1x1x1. 
    // The problem with this approach is that this only works well if there is only 1 local (and global) maximum AND if
    // by luck the representative point turns out to be chosen well.
    // However, since there are multiple local maxima and since we can be unlucky, there is the danger that we will choose the wrong region.
    // Different configurations of this strategy gave different results, but one result turned out to be the correct result.

    // Successful but unsatisfactory attempt #3:
    // A zoom in strategy:
    // Partition the search space into regions. For each region, select the middle point as region representative.
    // For the representing point, calculate the number of bots in range.
    // Take the one region with the highest result and zoom in on that one (but zoom in only by a factor 2 for example, 
    // whereas the partitioning factor was say 10x10x10. So effectively we only discard the edges).
    // Zoom in by increasing the zoom-factor and set the center of the new search space to the center of the region with highest result.
    // Partition the new search space again. Continue until region has size 1x1x1.
    // The problem with this approach is that this only works well if there is only 1 local (and global) maximum.
    // However, since there are multiple local maxima, there is the danger that we will zoom in on the wrong local maximum.
    // Different configurations of this strategy gave different results, but one result turned out to be the correct result.

    // Failed attempt #4:
    // An adjusted zoom in strategy, zooming in on multiple regions:
    // Partition the search space into regions. For each region, select the middle point as region representative.
    // For the representing point, calculate the number of bots in range.
    // Take the regions with the highest results and zoom in on all of those. Discard all other regions.
    // The problem with this approach is that we will start zooming on a lot of local maxima, too many (or too few).
    // Also this approach is unsatisfactory, since it requires configuration of when to zoom in and when not to, 
    // which is kind of arbitrary.
    