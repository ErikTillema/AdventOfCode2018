module Problem13

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Direction = | Up = 0 | Left = 1 | Down = 2 | Right = 3
    type NextTurn =  | TurnLeft = 0 | Straight = 1 | TurnRight = 2
    type Cart = { mutable position: int*int;
                  mutable movesDone: int;
                  mutable direction: Direction; 
                  mutable nextTurn: NextTurn;    }
    type State = { mutable carts: Cart list }

    let getDv direction =
        match direction with
        | Direction.Right -> (1,0) 
        | Direction.Left -> (-1,0) 
        | Direction.Up -> (0,-1)
        | Direction.Down -> (0,1)

    let getNextNextTurn (nextTurn: NextTurn) = enum<NextTurn>(((int nextTurn)+1)%3)

    let applyNextTurn nextTurn = 
        let turnLeft direction = enum<Direction>(((int direction)+1)%4)
        match nextTurn with
        | NextTurn.TurnLeft -> turnLeft
        | NextTurn.TurnRight -> turnLeft >> turnLeft >> turnLeft
        | NextTurn.Straight -> id

    let (++) (x,y) (dx,dy) = (x+dx,y+dy)

    let parseGrid input = 
        let sc = Scanner(input, false)
        let parseChar c = 
            match c with
            | '>' | '<' -> '-'
            | '^' | 'v' -> '|'
            | _         -> c
        let parseLine line = line |> Seq.map parseChar |> Seq.toArray
        sc.Lines |> Seq.map parseLine |> Seq.toArray

    let parseState input = 
        let sc = Scanner(input, false)
        let carts = 
            sc.Lines 
            |> Seq.indexed 
            |> Seq.collect (fun (y,line) -> 
                line 
                |> Seq.mapi (fun x c -> 
                    match c with
                    | '>' -> Some({ position = (x,y); movesDone = 0; nextTurn = NextTurn.TurnLeft; direction = Direction.Right; })
                    | '<' -> Some({ position = (x,y); movesDone = 0; nextTurn = NextTurn.TurnLeft; direction = Direction.Left; })
                    | '^' -> Some({ position = (x,y); movesDone = 0; nextTurn = NextTurn.TurnLeft; direction = Direction.Up; })
                    | 'v' -> Some({ position = (x,y); movesDone = 0; nextTurn = NextTurn.TurnLeft; direction = Direction.Down; })
                    | _ -> None
                ) 
                |> Seq.filter Option.isSome 
                |> Seq.map Option.get
            ) 
            |> Seq.toList
        { carts = carts }

    let getCollision state = 
        state.carts |> Seq.groupBy (fun c -> c.position) 
                    |> Seq.filter (fun g -> g |> snd |> Seq.length > 1) 
                    |> Seq.map fst |> Seq.tryHead

    let move (grid: char[][]) cart = 
        cart.position <- cart.position ++ (getDv cart.direction)
        let (nx,ny) = cart.position
        let newDirection, newNextTurn =
            match grid.[ny].[nx], cart.direction, cart.nextTurn with
            | '/' , Direction.Up, nt    -> Direction.Right, nt
            | '/' , Direction.Left, nt  -> Direction.Down, nt
            | '/' , Direction.Down, nt  -> Direction.Left, nt
            | '/' , Direction.Right, nt -> Direction.Up, nt
            | '\\', Direction.Down, nt  -> Direction.Right, nt
            | '\\', Direction.Left, nt  -> Direction.Up, nt
            | '\\', Direction.Up, nt    -> Direction.Left, nt
            | '\\', Direction.Right, nt -> Direction.Down, nt
            | '+' , d, nt -> applyNextTurn nt d, getNextNextTurn nt
            | _   , d, nt -> d, nt
        cart.direction <- newDirection
        cart.nextTurn <- newNextTurn
        cart.movesDone <- cart.movesDone + 1

    let rec moveUntilCollision grid state = 
        match getCollision state with
        | Some(pos) -> pos
        | None -> 
            let cartToMove = state.carts  |> Seq.minBy (fun c -> let (x,y) = c.position
                                                                 (c.movesDone,y,x) )  
            move grid cartToMove
            moveUntilCollision grid state

    let solveSilver input = 
        let grid = parseGrid input
        let state = parseState input
        moveUntilCollision grid state

    let solveGold input = 
        let grid = parseGrid input
        let state = parseState input

        let rec moveUntilLastCollision() =
            if state.carts.Length = 1 then 
                let lastCart = state.carts.Head
                move grid lastCart // move one last time, assuming that the one remaining cart still hasn't moved this tick yet (which is a bad assumption)
                lastCart.position
            else
                let collisionPos = moveUntilCollision grid state
                state.carts <- state.carts |> List.filter (fun c -> c.position <> collisionPos)
                moveUntilLastCollision()

        moveUntilLastCollision()