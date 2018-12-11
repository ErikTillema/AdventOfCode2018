module Problem8

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Node = { metadata: int array;
                  children: Node array }

    let parseInput input = 
        let sc = Scanner(input, false)
        let rec parseNode vals = 
            let childCount = vals |> Seq.head
            let metadataCount = vals |> Seq.head
            let children = [0..childCount-1] |> Seq.map (fun _ -> parseNode vals) |> Seq.toArray
            let metadata = vals |> Seq.take metadataCount |> Seq.toArray
            { metadata = metadata;
              children = children }
        parseNode (sc.Ints)

    let solveSilver input = 
        let root = parseInput input
        let rec getMetadataSum node = (node.metadata |> Array.sum) + (node.children |> Array.sumBy getMetadataSum)
        getMetadataSum root
    
    let solveGold input = 
        let root = parseInput input
        let rec getValue = 
            let cache = Dictionary<Node,int>()
            let calcValue node = 
                if node.children.Length = 0 then
                    (node.metadata |> Array.sum)
                else
                    node.metadata |> Seq.filter (fun i -> 1 <= i && i <= node.children.Length) 
                                  |> Seq.sumBy (fun i -> node.children.[i-1] |> getValue)
            (fun node ->
                if not(cache.ContainsKey(node)) then 
                    cache.[node] <- calcValue node
                cache.[node]
            )

        getValue root
