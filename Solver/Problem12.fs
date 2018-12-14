module Problem12

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type State = { mutable leftMostX: int;           // for example -2 if the left-most plant is at x=-2
                   mutable plantPositions: bool[] ; }

    let parseState initialLine = 
        let positions = initialLine |> Seq.map (fun c -> c = '#') |> Seq.toArray
        { leftMostX = 0; plantPositions = positions; }

    let parseRules rulesInput = 
        let parseRule line = 
            match line with
            | Regex "^(?<left>[\.#]+) => (?<right>[\.#])$" [ left; right ] -> 
                let group = left |> Seq.map (fun c -> c = '#') |> Seq.toArray
                let result = right.[0] = '#'
                (group,result) 
            | _ -> invalidOp "bad line"
        let sc = Scanner(rulesInput, false)
        sc.Lines |> Seq.map parseRule |> Seq.filter snd |> Seq.map fst |> Set.ofSeq

    let grow (rules: Set<bool[]>) state = 
        let hasPlant x = 
            if x < state.leftMostX then false
            elif x >= state.leftMostX + state.plantPositions.Length then false
            else state.plantPositions.[x - state.leftMostX]
        let getGroup x = 
            [| -2..2 |] |> Array.map (fun dx -> hasPlant (x+dx))
        let willHavePlant x = rules.Contains(getGroup x)
        let newStart = state.leftMostX - 2
        let newEnd = state.leftMostX + state.plantPositions.Length-1 + 2
        let newPlantPositions = [| newStart..newEnd |] |> Array.map willHavePlant
        let skipStart = newPlantPositions |> Seq.takeWhile (not) |> Seq.length
        let skipEnd = newPlantPositions |> Seq.rev |> Seq.takeWhile (not) |> Seq.length // Fails if there are no more plants
        state.plantPositions <- newPlantPositions |> Seq.skip skipStart |> Seq.take (newPlantPositions.Length - skipStart - skipEnd) |> Seq.toArray
        state.leftMostX <- newStart + skipStart
        ()

    let getScore state = 
        state.plantPositions |> Seq.mapi (fun i hasPlant -> (i,hasPlant)) |> Seq.filter snd |> Seq.sumBy (fun (i,_) -> i + state.leftMostX)

    let solveSilver initialLine rulesInput = 
        let mutable state = parseState initialLine
        let rules = parseRules rulesInput
        for _ in [1..20] do grow rules state
        getScore state

    let solveGold initialLine rulesInput = 
        let mutable state = parseState initialLine
        let rules = parseRules rulesInput
        for _ in [1..100] do grow rules state 
        // After 100 generations, the plants just move 1 position to the right at every generation.
        // So the score increases with the number of plants for every generation.
        //for _ in 1..10 do 
        //    for _ in [1..1] do grow rules state
        //    state.plantPositions |> Array.map (fun p -> if p then '#' else '.') |> String |> printfn "%s"
        let plants = state.plantPositions |> Seq.filter (id) |> Seq.length
        (int64 (getScore state)) + (int64 plants) * (50_000_000_000L - 100L)
        