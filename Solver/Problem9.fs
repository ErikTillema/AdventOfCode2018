module Problem9

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let solveSilver players lastMarble = 
        let list = LinkedList<int64>([0L; 1L])
        let score = Array.create players 0L
        let mutable cur = list.Last
        let mutable curPlayer = 1
        let moveForward() = 
            cur <- 
                if cur.Next = null then list.First
                else cur.Next
        let moveBackwards() = 
            cur <-
                if cur.Previous = null then list.Last
                else cur.Previous
        for i in 2L..lastMarble do
            if i%23L = 0L then
                for _ in 1..7 do moveBackwards()
                score.[curPlayer] <- score.[curPlayer] + i + (cur.Value)
                let newCur = cur.Next
                list.Remove(cur)
                cur <- newCur
            else
                moveForward()
                list.AddAfter(cur, i) |> ignore // O(1)
                moveForward()
            curPlayer <- (curPlayer+1)%players
        
        score |> Array.max
    
    let solveGold players lastMarble = solveSilver players (lastMarble*100L)
