module Problem15

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type CreatureType = Elf | Goblin

    type Creature = {
        creatureType: CreatureType;
        attackPower: int;
        mutable hitPoints: int;
        mutable position: int*int;
    }

    type GridPosition =
        | Wall 
        | Free 
        | Occupied of Creature

    type State = {
        creatures: Creature list;
        grid: GridPosition[,];
    }

    let parseState w h input elfAttackPower =
        let grid = Array2D.create w h Wall
        let mutable creatures = []
        let sc = Scanner(input, false)
        sc.Lines 
            |> Seq.iteri (fun y line ->
                line |> Seq.iteri (fun x c ->
                    match c with
                    | '#' -> grid.[x,y] <- Wall
                    | '.' -> grid.[x,y] <- Free
                    | 'G' -> 
                        let creature = { creatureType = Goblin; hitPoints = 200; position = (x,y); attackPower = 3 }
                        creatures <- creature :: creatures
                        grid.[x,y] <- Occupied(creature)
                    | 'E' -> 
                        let creature = { creatureType = Elf; hitPoints = 200; position = (x,y); attackPower = elfAttackPower }
                        creatures <- creature :: creatures
                        grid.[x,y] <- Occupied(creature)
                    | _ -> invalidOp "bad character"
                )
            )
        { creatures = creatures; grid = grid }

    let isAlive creature = creature.hitPoints > 0

    let playRound w h state = 
        let getNeighbours (x,y) = 
            [ (x+1,y); (x-1,y); (x,y+1); (x,y-1) ] |> Seq.filter (fun (x,y) -> 0 <= x && x < w && 0 <= y && y < h && state.grid.[x,y] <> Wall)
        let canMoveTo (x,y) =
            match state.grid.[x,y] with
            | Free -> true
            | _ -> false
        let isTarget attacker (x,y) =
            match state.grid.[x,y] with 
            | Occupied(target) when target.creatureType <> attacker.creatureType -> true
            | _ -> false 
        let getTarget (x,y) =
            match state.grid.[x,y] with
            | Occupied(target) -> target
            | _ -> invalidOp "No creature was found"
        let isAdjacentToTarget attacker position = 
            getNeighbours position |> Seq.exists (isTarget attacker)
        let isAttacking attacker = 
            isAdjacentToTarget attacker attacker.position
        let findTarget attacker = 
            // floodfill, find nearest position adjacent to a target.
            let queue = Queue<int*int>()
            let distance = Dictionary<int*int,int>()
            let mutable targetDistance = None
            let mutable targets = []
            queue.Enqueue(attacker.position)
            distance.[attacker.position] <- 0
            while queue.Count > 0 do
                let curPos = queue.Dequeue()
                let d = distance.[curPos]
                for nb in getNeighbours curPos |> Seq.filter canMoveTo do
                    if not(distance.ContainsKey(nb)) then
                        distance.[nb] <- d+1
                        queue.Enqueue(nb)
                        if isAdjacentToTarget attacker nb then
                            match targetDistance with
                            | None -> 
                                targetDistance <- Some(d+1)
                                targets <- nb :: targets
                            | Some(td) when td = d+1 -> 
                                targets <- nb :: targets
                            | _ -> ()
            
            targets |> Seq.sortBy (fun (x,y) -> (y,x)) |> Seq.tryHead

        let findStep attacker targetPos = 
            // floodfill from targetPos, find first step to take for attacker
            let queue = Queue<int*int>()
            let distance = Dictionary<int*int,int>()
            queue.Enqueue(targetPos)
            distance.[targetPos] <- 0
            while queue.Count > 0 do
                let curPos = queue.Dequeue()
                let d = distance.[curPos]
                for nb in getNeighbours curPos |> Seq.filter canMoveTo do
                    if not(distance.ContainsKey(nb)) then
                        distance.[nb] <- d+1
                        queue.Enqueue(nb)
            
            attacker.position |> getNeighbours 
                |> Seq.filter canMoveTo 
                |> Seq.filter (fun (x,y) -> distance.ContainsKey((x,y)))
                |> Seq.sortBy (fun (x,y) -> (distance.[(x,y)],y,x)) 
                |> Seq.head

        let creaturesInOrder = 
            state.creatures 
            |> Seq.filter isAlive 
            |> Seq.sortBy (fun c -> let (x,y) = c.position
                                    (y,x)                    )
            |> Seq.toArray

        for creature in creaturesInOrder do
            if isAlive creature then
                if not(isAttacking creature) then
                    // move
                    match findTarget creature with
                    | None -> ()
                    | Some(targetPos) ->
                        let (ox,oy) = creature.position
                        let (nx,ny) = findStep creature targetPos
                        state.grid.[ox,oy] <- Free
                        state.grid.[nx,ny] <- Occupied(creature)
                        creature.position <- (nx,ny)
                
                if isAttacking creature then
                    // attack
                    let target = 
                        creature.position |> getNeighbours
                        |> Seq.filter (isTarget creature) 
                        |> Seq.map getTarget
                        |> Seq.sortBy (fun c -> (c.hitPoints, snd c.position, fst c.position))
                        |> Seq.head
                    target.hitPoints <- target.hitPoints - creature.attackPower
                    if not(isAlive target) then 
                        let (tx,ty) = target.position
                        state.grid.[tx,ty] <- Free

    let rec playRounds acc w h state =
        let aliveCreatures = state.creatures |> Seq.filter isAlive |> Seq.toArray
        let creatureType = aliveCreatures.[0].creatureType
        if aliveCreatures |> Seq.forall (fun c -> c.creatureType = creatureType) then
            acc
        else
            playRound w h state
            playRounds (acc+1) w h state

    let getResult w h state = 
        let roundsPlayed = playRounds 0 w h state
        let hitPoints = state.creatures |> Seq.filter isAlive |> Seq.sumBy (fun c -> c.hitPoints)
        hitPoints * (roundsPlayed - 1)

    let solveSilver w h input = 
        let state = parseState w h input 3
        getResult w h state

    let solveGold w h input = 
        let getElfCount state = state.creatures |> Seq.filter isAlive |> Seq.filter (fun c -> c.creatureType = Elf) |> Seq.length
        let originalElfCount = getElfCount (parseState w h input 3)

        let rec bsearch start endd = 
            let isOk elfAttackPower =
                let state = parseState w h input elfAttackPower
                playRounds 0 w h state |> ignore
                getElfCount state = originalElfCount
            if start = endd then start
            else 
                let mid = (start+endd)/2
                if isOk mid then
                    bsearch start mid
                else
                    bsearch (mid+1) endd

        let minElfAttackPower = bsearch 0 100
        let state = parseState w h input minElfAttackPower
        getResult w h state