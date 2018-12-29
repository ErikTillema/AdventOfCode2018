module Problem21

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let getValuesOfAWithTerminatingProgram() =
        let mutable e = 0L
        seq {
            while true do
                let mutable b = e ||| 65536L
                e <- 2024736L

                while b >= 1L do
                    e <- e + (b &&& 255L)
                    e <- e &&& 16777215L
                    e <- e * 65899L
                    e <- e &&& 16777215L
                    b <- b / 256L
            
                yield e
        }

    let solveSilver() = 
        getValuesOfAWithTerminatingProgram() |> Seq.head
        
    let solveGold() = 
        let getAllValuesOfAWithTerminatingProgram() = 
            let cache = HashSet<int64>()
            getValuesOfAWithTerminatingProgram() |> Seq.takeWhile (fun v -> cache.Add(v))
        getAllValuesOfAWithTerminatingProgram() |> Seq.last
