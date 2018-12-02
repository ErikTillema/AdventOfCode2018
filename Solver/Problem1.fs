module Problem1

    open System.Collections.Generic
    open Util

    let parseNumbers input = 
        let sc = Scanner(input, false)
        sc.Ints |> Array.ofSeq
    
    type State = { seen: HashSet<int>;
                   mutable curPos: int;
                   mutable curSum: int;
                   mutable found: bool; }

    let rec doSteps (numbers: int[]) state = 
        let doStep() = 
            state.curSum <- state.curSum + numbers.[state.curPos]
            state.curPos <- (state.curPos + 1) % (numbers.Length)
            if state.seen.Contains(state.curSum) then
                state.found <- true
            else 
                state.seen.Add(state.curSum) |> ignore
        
        if not state.found then
            doStep()
            doSteps numbers state

    let solveSilver input = 
        let numbers = parseNumbers input
        numbers |> Array.sum

    let solveGold input = 
        let numbers = parseNumbers input
        let state = { seen = HashSet( [0] );
                      curPos = 0;
                      curSum = 0;
                      found = false; }
        doSteps numbers state
        state.curSum

        