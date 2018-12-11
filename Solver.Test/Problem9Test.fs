namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem9
open System.Resources

type Problem9Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 9 25L |> should equal 32L
         solveSilver 13 7999L |> should equal 146373L
         solveSilver 30 5807L |> should equal 37305L

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 418 71339L |> should equal 412127L

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 418 71339L |> should equal 3482394794L

     