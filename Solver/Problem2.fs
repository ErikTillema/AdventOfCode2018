module Problem2

    open System
    open Util
    open SeqExt
    
    let solveSilver input = 
        let getResult (boxId: string) = 
            let count = Array.create 26 0
            boxId |> Seq.map (fun c -> (int c) - (int 'a')) |> Seq.iter (fun x -> count.[x] <- count.[x] + 1)
            let has2 = count |> Array.exists ((=) 2)
            let has3 = count |> Array.exists ((=) 3)
            (has2,has3)
            
        let sc = Scanner(input, false)
        let results = sc.Lines |> Seq.map getResult |> Array.ofSeq
        let count2 = results |> Seq.filter fst |> Seq.length
        let count3 = results |> Seq.filter snd |> Seq.length
        count2 * count3

    let solveGold input = 
        let sc = Scanner(input, false)
        let ids = sc.Lines |> Seq.map (fun s -> s.ToCharArray()) |> Seq.toArray
        let l = ids |> Array.length

        let isOk [|i;j|] =
            Seq.zip ids.[i] ids.[j] |> Seq.filter (fun (c1,c2) -> c1 <> c2) |> Seq.truncate 2 |> Seq.length = 1

        let [|a;b|] = getChooseCombinations 2 [0..l-1] |> Seq.find isOk
        Seq.zip ids.[a] ids.[b] |> Seq.filter (fun (c1,c2) -> c1 = c2) |> Seq.map fst |> Seq.toArray |> String

                
        