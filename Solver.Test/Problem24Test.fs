namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem24

type Problem24Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
        solveSilver true |> should equal 5216
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver false |> should equal 16086
    
    [<Fact>]
    member x.solveGold_works () = 
        solveGold true |> should equal 51

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold false |> should equal 3957
    
