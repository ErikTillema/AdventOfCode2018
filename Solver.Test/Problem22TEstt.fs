namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem22

type Problem22Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
        solveSilver 510 (10,10) |> should equal 114
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 11991 (6,797) |> should equal 5622
    
    [<Fact>]
    member x.solveGold_works () = 
        solveGold 510 (10,10) |> should equal 45

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 11991 (6,797) |> should equal 1089
    
