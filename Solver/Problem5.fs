module Problem5

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let invert (c: char) = 
        if 'a' <= c && c <= 'z' then
            char ((int c) - (int 'a') + (int 'A'))
        else 
            char ((int c) - (int 'A') + (int 'a'))

    let solve (list: LinkedList<char>) = 
        let mutable cur = list.First
        let rec simplify() = 
            let rec findPair() = 
                if cur = null || cur.Next = null then
                    None
                else 
                    match cur.Next.Value with
                    | v when v = invert cur.Value -> Some(1)
                    | _ -> 
                        cur <- cur.Next
                        findPair()
            
            match findPair() with
            | None -> ()
            | _ -> 
                let newCur = if cur.Previous = null then
                                 cur.Next.Next
                             else
                                 cur.Previous
                list.Remove(cur.Next)
                list.Remove(cur)
                cur <- newCur
                simplify()

        simplify()
        list.Count

    let getList (skipChar: char option) input = 
        let list = LinkedList<char>()
        let sc = Scanner(input, false)
        sc.Next().Value 
            |> Seq.filter (fun c -> skipChar.IsNone || (c <> skipChar.Value && c <> invert skipChar.Value))
            |> Seq.iter (fun c -> list.AddLast(c) |> ignore)
        list

    let solveSilver input = 
        input |> getList None |> solve

    let solveGold input = 
        ['a'..'z'] |> Seq.map (fun c -> input |> getList (Some(c)) |> solve) |> Seq.min