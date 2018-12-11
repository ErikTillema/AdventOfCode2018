namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem11
open System.Resources

type Problem11Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 18 |> should equal (33,45)
         solveSilver 42 |> should equal (21,61)
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold 18 |> should equal (90,269,16)

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 9221 |> should equal (20,77)

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 9221 |> should equal (143,57,10)

     