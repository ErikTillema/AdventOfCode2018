module Problem19

    open System
    open Util
    open SeqExt
    open System.Collections.Generic
    
    type Value = Value of int
    type Register = Register of int
    type Input = 
        | Addr of Register * Register
        | Addi of Register * Value
        | Mulr of Register * Register
        | Muli of Register * Value
        | Banr of Register * Register
        | Bani of Register * Value
        | Borr of Register * Register
        | Bori of Register * Value
        | Setr of Register
        | Seti of Value
        | Gtir of Value    * Register
        | Gtri of Register * Value
        | Gtrr of Register * Register
        | Eqir of Value    * Register
        | Eqri of Register * Value
        | Eqrr of Register * Register
    type Instruction = Instruction of Input * Register

    type State = {  mutable pos: int;
                    boundedRegister: Register;
                    registerValues: int[]; 
                    instructions: Instruction[]; }

    let doStep state = 
        let getRegisterValue (Register(i)) = state.registerValues.[i]
        let setRegisterValue (Register(i)) value =
            state.registerValues.[i] <- value
        let evaluate (Instruction(input,outputRegister)) = 
            let value =
                match input with
                | Addr(ra,rb)       -> getRegisterValue ra + getRegisterValue rb
                | Addi(ra,Value(b)) -> getRegisterValue ra + b
                | Mulr(ra,rb)       -> getRegisterValue ra * getRegisterValue rb
                | Muli(ra,Value(b)) -> getRegisterValue ra * b
                | Banr(ra,rb)       -> getRegisterValue ra &&& getRegisterValue rb
                | Bani(ra,Value(b)) -> getRegisterValue ra &&& b
                | Borr(ra,rb)       -> getRegisterValue ra ||| getRegisterValue rb
                | Bori(ra,Value(b)) -> getRegisterValue ra ||| b
                | Setr(ra)          -> getRegisterValue ra
                | Seti(Value(a))    -> a
                | Gtir(Value(a),rb) -> if a > getRegisterValue rb then 1 else 0
                | Gtri(ra,Value(b)) -> if getRegisterValue ra > b then 1 else 0
                | Gtrr(ra,rb)       -> if getRegisterValue ra > getRegisterValue rb then 1 else 0
                | Eqir(Value(a),rb) -> if a = getRegisterValue rb then 1 else 0
                | Eqri(ra,Value(b)) -> if getRegisterValue ra = b then 1 else 0
                | Eqrr(ra,rb)       -> if getRegisterValue ra = getRegisterValue rb then 1 else 0
            setRegisterValue outputRegister value
        
        let instruction = state.instructions.[state.pos]
        evaluate instruction
        state.pos <- (getRegisterValue state.boundedRegister + 1)
        setRegisterValue state.boundedRegister state.pos

    let rec doSteps state = 
        if state.pos >= state.instructions.Length then ()
        else 
            doStep state
            //printfn "pos=%d registers=[ %d, %d, %d, %d, %d, %d ]" state.pos state.registerValues.[0] state.registerValues.[1] state.registerValues.[2] state.registerValues.[3] state.registerValues.[4] state.registerValues.[5]
            doSteps state

    let parseInstructions input = 
        let parseLine line =
            match line with
            | Regex "^addr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Addr(Register(int a),Register(int b)), Register(int c))
            | Regex "^addi (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Addi(Register(int a),Value(int b)),    Register(int c))
            | Regex "^mulr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Mulr(Register(int a),Register(int b)), Register(int c))
            | Regex "^muli (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Muli(Register(int a),Value(int b)),    Register(int c))
            | Regex "^banr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Banr(Register(int a),Register(int b)), Register(int c))
            | Regex "^bani (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Bani(Register(int a),Value(int b)),    Register(int c))
            | Regex "^borr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Borr(Register(int a),Register(int b)), Register(int c))
            | Regex "^bori (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Bori(Register(int a),Value(int b)),    Register(int c))
            | Regex "^setr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; _; c ] -> Instruction(Setr(Register(int a)),                 Register(int c))
            | Regex "^seti (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; _; c ] -> Instruction(Seti(Value(int a)),                    Register(int c))
            | Regex "^gtir (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Gtir(Value(int a),Register(int b)),    Register(int c))
            | Regex "^gtri (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Gtri(Register(int a),Value(int b)),    Register(int c))
            | Regex "^gtrr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Gtrr(Register(int a),Register(int b)), Register(int c))
            | Regex "^eqir (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Eqir(Value(int a),Register(int b)),    Register(int c))
            | Regex "^eqri (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Eqri(Register(int a),Value(int b)),    Register(int c))
            | Regex "^eqrr (?<a>\d+) (?<b>\d+) (?<c>\d+)$" [ a; b; c ] -> Instruction(Eqrr(Register(int a),Register(int b)), Register(int c))
            | _ -> invalidOp "bad line"
        let sc = Scanner(input, false)
        sc.Lines |> Seq.filter (fun l -> not(l.StartsWith("#"))) |> Seq.map parseLine |> Seq.toArray

    let solveSilver boundedRegister input = 
        let state = { pos = 0;
                      boundedRegister = Register(boundedRegister);
                      registerValues = Array.create 6 0;
                      instructions = parseInstructions input; }
        doSteps state
        state.registerValues.[0]

    let solveGoldSlow boundedRegister input = 
        let state = { pos = 0;
                      boundedRegister = Register(boundedRegister);
                      registerValues = [| 1; 0; 0; 0; 0; 0 |]
                      instructions = parseInstructions input; }
        doSteps state
        state.registerValues.[0]

    let solveGold _ _ = 
        let getDivisors n = 
            let sqrt n = n |> float |> sqrt |> floor |> int
            seq {
                for i in 1..sqrt n do
                    if n%i=0 then
                        yield i
                        if i <> (n/i) then yield (n/i)
            } |> Seq.sort |> Seq.toList
        let d = 10_551_288
        d |> getDivisors |> Seq.sum
