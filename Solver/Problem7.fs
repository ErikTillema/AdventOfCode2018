module Problem7

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Node = { name: char 
                  neighboursFrom: LinkedList<Node>;
                  neighboursTo: LinkedList<Node> }

    let parseInput input = 
        let nodes = Dictionary<char, Node>()
        let getNode c = 
            if not(nodes.ContainsKey(c)) then 
                let newNode = { name = c;
                                neighboursFrom = LinkedList<Node>();
                                neighboursTo = LinkedList<Node>() }
                nodes.Add(c, newNode)
            nodes.[c]
        let parseLine line = 
            match line with
            | Regex "^Step (?<from>[A-Z]) must be finished before step (?<to>[A-Z]) can begin.$" [ from; too ] -> 
                let nodeFrom = getNode from.[0]
                let nodeTo = getNode too.[0]
                nodeFrom.neighboursTo.AddLast(nodeTo) |> ignore
                nodeTo.neighboursFrom.AddLast(nodeFrom) |> ignore
            | _ -> invalidOp "bad line"
        let sc = Scanner(input, false)
        sc.Lines |> Seq.iter (parseLine)
        nodes
    
    let getComparer (getObject: 'a -> 'b when 'b :> IComparable) = 
        Comparer<'a>.Create(Comparison<'a>(fun a1 a2 -> getObject(a1).CompareTo(getObject(a2))))

    let solveSilver input = 
        let nodes = parseInput input
        // maintain nodes with indegree zero
        let inDegree node = node.neighboursFrom.Count
        let starts = SortedSet (nodes.Values |> Seq.filter (fun n -> inDegree n = 0), getComparer (fun n -> n.name))

        let rec getResult acc = 
            if starts.Count = 0 then 
                acc |> List.rev |> List.toArray |> String
            else
                let nextNode = starts.Min
                starts.Remove(nextNode) |> ignore
                for nb in nextNode.neighboursTo do
                    nb.neighboursFrom.Remove(nextNode) |> ignore
                    if inDegree nb = 0 then starts.Add(nb) |> ignore
                getResult (nextNode.name :: acc)

        getResult []

    let solveGold extraDuration workers input = 
        let nodes = parseInput input
        // maintain nodes with indegree zero
        let inDegree node = node.neighboursFrom.Count
        let starts = SortedSet (nodes.Values |> Seq.filter (fun n -> inDegree n = 0), getComparer (fun n -> n.name))

        // let's not brute-force it but do it a bit smarter.
        // Maintain state, consider only relevant times.
        // - per time, remember which workers become idle
        // - per time, remember which nodes become done
        // - instead of looping over all time, keep a SortedSet<int> of only relevant times.

        let nodesDoneAt = Dictionary<int,Node list>() // at time t, which Nodes are just done?
        let workersDoneAt = Dictionary<int,int list>() // at time t, which workers are just done?
        let idleWorkers = SortedSet<int>( [0..workers-1] )
        let relevantTimes = SortedSet<int>()
        let addRelevantTime t =
            relevantTimes.Add(t) |> ignore
            workersDoneAt.Add(t, [])
            nodesDoneAt.Add(t, [])
        addRelevantTime 0

        let getReadyNodes() = starts
        let getDuration node = 
            extraDuration + 1 + (int node.name) - (int 'A')

        let rec solve t nodesDoneTotal =
            // which Nodes are done now?
            let nodesDoneNow = nodesDoneAt.[t] |> Seq.length
            for node in nodesDoneAt.[t] do
                for nb in node.neighboursTo do
                    nb.neighboursFrom.Remove(node) |> ignore
                    if inDegree nb = 0 then starts.Add(nb) |> ignore

            // which workes became idle?
            for w in workersDoneAt.[t] do
                idleWorkers.Add(w) |> ignore

            if nodesDoneTotal + nodesDoneNow = nodes.Count then t
            else
                // which workers are idle?
                for w in idleWorkers |> Seq.toList do
                    // find a node that can be picked up
                    match getReadyNodes() |> Seq.tryHead with
                    | None -> ()
                    | Some(node) -> 
                        let doneAt = t + getDuration node
                        addRelevantTime doneAt
                        workersDoneAt.[doneAt] <- w :: workersDoneAt.[doneAt]
                        nodesDoneAt.[doneAt] <- node :: nodesDoneAt.[doneAt]
                        starts.Remove(node) |> ignore
                        idleWorkers.Remove(w) |> ignore
                
                relevantTimes.Remove(t) |> ignore
                solve (relevantTimes.Min) (nodesDoneTotal+nodesDoneNow)

        solve 0 0
