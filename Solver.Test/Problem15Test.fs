namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem15
open System.Resources

type Problem15Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "" |> should equal 42
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver "" |> should equal 42
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold "" |> should equal 42
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold "" |> should equal 42
    
