namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem14
open System.Resources

type Problem14Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 9 |> should equal "5158916779"
         solveSilver 5 |> should equal "0124515891"
         solveSilver 18 |> should equal "9251071085"
         solveSilver 2018 |> should equal "5941429882"
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 825401 |> should equal "6289129761"
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold 5 51589 |> should equal 9
         solveGold 5 01245 |> should equal 5
         solveGold 5 92510 |> should equal 18
         solveGold 5 59414 |> should equal 2018
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 6 825401 |> should equal 20207075
    
