namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem3
open System.Resources

type Problem3Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (  "#1 @ 1,3: 4x4\n"
                      + "#2 @ 3,1: 4x4\n"
                      + "#3 @ 5,5: 2x2" ) |> should equal 4

    [<Fact>]
    member x.solveGold_works () = 
         solveGold (    "#1 @ 1,3: 4x4\n"
                      + "#2 @ 3,1: 4x4\n"
                      + "#3 @ 5,5: 2x2" ) |> should equal 3

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem3.in")) |> should equal 106501

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem3.in")) |> should equal 632

     