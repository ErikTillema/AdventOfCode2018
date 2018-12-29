module Problem16

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Value = Value of int
    type Register = Register of int

    type OperationType = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr

    let allOperationTypes = [OperationType.Addr; OperationType.Addi; OperationType.Mulr; OperationType.Muli; OperationType.Banr; OperationType.Bani; OperationType.Borr; OperationType.Bori; OperationType.Setr; OperationType.Seti; OperationType.Gtir; OperationType.Gtri; OperationType.Gtrr; OperationType.Eqir; OperationType.Eqri; OperationType.Eqrr; ]

    type Operation = 
        | Addr of Register * Register * Register
        | Addi of Register * Value    * Register
        | Mulr of Register * Register * Register
        | Muli of Register * Value    * Register
        | Banr of Register * Register * Register
        | Bani of Register * Value    * Register
        | Borr of Register * Register * Register
        | Bori of Register * Value    * Register
        | Setr of Register * Register
        | Seti of Value    * Register
        | Gtir of Value    * Register * Register
        | Gtri of Register * Value    * Register
        | Gtrr of Register * Register * Register
        | Eqir of Value    * Register * Register
        | Eqri of Register * Value    * Register
        | Eqrr of Register * Register * Register

    type State = { registerValues: int[]; }

    let getRegisterValue state (Register(i)) =
        state.registerValues.[i]

    let setRegisterValue state (Register(i)) value =
        state.registerValues.[i] <- value

    let evaluate operation state = 
        match operation with
        | Addr(ra,rb,rc)       -> setRegisterValue state rc (getRegisterValue state ra + getRegisterValue state rb)
        | Addi(ra,Value(b),rc) -> setRegisterValue state rc (getRegisterValue state ra + b)
        | Mulr(ra,rb,rc)       -> setRegisterValue state rc (getRegisterValue state ra * getRegisterValue state rb)
        | Muli(ra,Value(b),rc) -> setRegisterValue state rc (getRegisterValue state ra * b)
        | Banr(ra,rb,rc)       -> setRegisterValue state rc (getRegisterValue state ra &&& getRegisterValue state rb)
        | Bani(ra,Value(b),rc) -> setRegisterValue state rc (getRegisterValue state ra &&& b)
        | Borr(ra,rb,rc)       -> setRegisterValue state rc (getRegisterValue state ra ||| getRegisterValue state rb)
        | Bori(ra,Value(b),rc) -> setRegisterValue state rc (getRegisterValue state ra ||| b)
        | Setr(ra,rc)          -> setRegisterValue state rc (getRegisterValue state ra)
        | Seti(Value(a),rc)    -> setRegisterValue state rc a
        | Gtir(Value(a),rb,rc) -> setRegisterValue state rc (if a > getRegisterValue state rb then 1 else 0)
        | Gtri(ra,Value(b),rc) -> setRegisterValue state rc (if getRegisterValue state ra > b then 1 else 0)
        | Gtrr(ra,rb,rc)       -> setRegisterValue state rc (if getRegisterValue state ra > getRegisterValue state rb then 1 else 0)
        | Eqir(Value(a),rb,rc) -> setRegisterValue state rc (if a = getRegisterValue state rb then 1 else 0)
        | Eqri(ra,Value(b),rc) -> setRegisterValue state rc (if getRegisterValue state ra = b then 1 else 0)
        | Eqrr(ra,rb,rc)       -> setRegisterValue state rc (if getRegisterValue state ra = getRegisterValue state rb then 1 else 0)

    let isMatch before after operation = 
        let state = { registerValues = before |> Array.copy }
        evaluate operation state
        state.registerValues = after

    let getOperationType operation = 
        match operation with
        | Addr(_,_,_) -> OperationType.Addr
        | Addi(_,_,_) -> OperationType.Addi
        | Mulr(_,_,_) -> OperationType.Mulr
        | Muli(_,_,_) -> OperationType.Muli
        | Banr(_,_,_) -> OperationType.Banr
        | Bani(_,_,_) -> OperationType.Bani
        | Borr(_,_,_) -> OperationType.Borr
        | Bori(_,_,_) -> OperationType.Bori
        | Setr(_,_) -> OperationType.Setr
        | Seti(_,_) -> OperationType.Seti
        | Gtir(_,_,_) -> OperationType.Gtir
        | Gtri(_,_,_) -> OperationType.Gtri
        | Gtrr(_,_,_) -> OperationType.Gtrr
        | Eqir(_,_,_) -> OperationType.Eqir
        | Eqri(_,_,_) -> OperationType.Eqri
        | Eqrr(_,_,_) -> OperationType.Eqrr

    let getOperation (operationInput: int[]) operationType = 
        match operationType with
        | OperationType.Addr -> Addr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Addi -> Addi(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Mulr -> Mulr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Muli -> Muli(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Banr -> Banr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Bani -> Bani(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Borr -> Borr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Bori -> Bori(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Setr -> Setr(Register(operationInput.[0]), Register(operationInput.[2]))
        | OperationType.Seti -> Seti(Value(operationInput.[0]), Register(operationInput.[2]))
        | OperationType.Gtir -> Gtir(Value(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Gtri -> Gtri(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Gtrr -> Gtrr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Eqir -> Eqir(Value(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Eqri -> Eqri(Register(operationInput.[0]), Value(operationInput.[1]), Register(operationInput.[2]))
        | OperationType.Eqrr -> Eqrr(Register(operationInput.[0]), Register(operationInput.[1]), Register(operationInput.[2]))

    let getOperations (operationInput: int[]) = 
        allOperationTypes |> List.map (getOperation operationInput)

    let countMatches before after operationInput = 
        getOperations operationInput |> Seq.filter (isMatch before after) |> Seq.length

    let parseBlocks input = 
        let sc = Scanner(input, false)
        let parseBlock (lines: String[]) =
            let before =
                match lines.[0] with
                | Regex "^Before:[ ]*\[(?<val1>\d+), (?<val2>\d+), (?<val3>\d+), (?<val4>\d+)\]$" [ val1; val2; val3; val4 ] -> [| int val1; int val2; int val3; int val4 |]
                | _ -> invalidOp "bad line1"
            let after =
                match lines.[2] with
                | Regex "^After:[ ]*\[(?<val1>\d+), (?<val2>\d+), (?<val3>\d+), (?<val4>\d+)\]$" [ val1; val2; val3; val4 ] -> [| int val1; int val2; int val3; int val4 |]
                | _ -> invalidOp "bad line1"
            let sc2 = Scanner(lines.[1], false)
            let opCode = sc2.Ints |> Seq.head |> int
            let operationInput = sc2.Ints |> Seq.toArray
            (opCode,before, after, operationInput)
        sc.Lines |> Seq.chunkBySize 4 |> Seq.map parseBlock

    let solveSilver input = 
        parseBlocks input |> Seq.filter (fun (_,b,a,oi) -> countMatches b a oi >= 3) |> Seq.length
    
    let solveGold input input2 = 
        let map = Dictionary<int,HashSet<OperationType>>() // opCode -> possible operation types
        for i in 0..15 do map.[i] <- HashSet<OperationType>(allOperationTypes)
        let updateMap (opCode,before,after,operationInput) = 
            for operation in getOperations operationInput do
                if not(isMatch before after operation) then
                    map.[opCode].Remove(getOperationType operation) |> ignore
        parseBlocks input |> Seq.iter updateMap

        // simplify:
        let rec simplify() =
            let dones = map |> Seq.filter (fun kvp -> kvp.Value.Count = 1) |> Seq.map (fun kvp -> kvp.Value |> Seq.head) |> Seq.toArray
            if dones.Length = 16 then ()
            else 
                for i in 0..15 do
                    if map.[i].Count > 1 then
                        for d in dones do   
                            map.[i].Remove(d) |> ignore
                simplify()
        simplify()
        let dones = map |> Seq.filter (fun kvp -> kvp.Value.Count = 1) |> Seq.map (fun kvp -> kvp.Value |> Seq.head) |> Seq.toArray

        let sc = Scanner(input2,false)
        let state = { registerValues = [|0;0;0;0|] }
        let evaluateLine line = 
            let sc2 = Scanner(line, false)
            let opCode = sc2.Ints |> Seq.head
            let operationInput = sc2.Ints |> Seq.toArray
            let operation = getOperation operationInput dones.[opCode]
            evaluate operation state

        sc.Lines |> Seq.iter evaluateLine
        state.registerValues.[0]
