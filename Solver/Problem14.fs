module Problem14

    open System
    open Util
    open SeqExt
    open System.Collections.Generic
    open System

    type State = { vals: List<int>;
                   mutable pos1: int;
                   mutable pos2: int;
                   mutable tail: int; // Number representing the last X digits in vals.
                   mutable leftOver: int option; } // Value of the second digit in case 2 digits are added at once (so when sum = 16 for example, leftOver = 6)

    let solveSilver steps = 
        let state = { vals = List<int>([3;7]); pos1 = 0; pos2 = 1; tail = 37; leftOver = None; }
        let rec growUntilSize n =
            if state.vals.Count >= n then ()
            else 
                let sum = state.vals.[state.pos1] + state.vals.[state.pos2]
                if sum >= 10 then state.vals.Add((sum/10)%10) |> ignore
                state.vals.Add(sum%10) |> ignore
                state.pos1 <- (state.pos1 + 1 + state.vals.[state.pos1])%state.vals.Count
                state.pos2 <- (state.pos2 + 1 + state.vals.[state.pos2])%state.vals.Count
                growUntilSize n

        growUntilSize (steps+10)
        state.vals |> Seq.skip steps |> Seq.take 10 |> Seq.map string |> String.concat ""

    let solveGold targetLength target = 
        let m = Math.Pow(10.0, float (targetLength-1)) |> int
        let state = { vals = List<int>([3;7]); pos1 = 0; pos2 = 1; tail = 37; leftOver = None; }

        let updateState digit leftOver movePositions = 
            state.vals.Add(digit) |> ignore
            state.tail <- (state.tail%m)*10 + digit
            state.leftOver <- leftOver
            if movePositions then
                state.pos1 <- (state.pos1 + 1 + state.vals.[state.pos1])%state.vals.Count
                state.pos2 <- (state.pos2 + 1 + state.vals.[state.pos2])%state.vals.Count

        let rec growUntilTarget() =
            if state.tail = target then state.vals.Count - targetLength
            else 
                match state.leftOver with
                | Some(d) -> updateState d None true
                | None ->
                    let sum = state.vals.[state.pos1] + state.vals.[state.pos2]
                    if sum >= 10 then 
                        updateState ((sum/10)%10) (Some(sum%10)) false
                    else 
                        updateState (sum%10) None true
                growUntilTarget()

        growUntilTarget()
