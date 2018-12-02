namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem1
open System.Resources

type Problem1Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "+1 +1 +1" |> should equal 3
         solveSilver "+1 +1 -2" |> should equal 0
         solveSilver "-1 -2 -3" |> should equal -6

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "+1 -1" |> should equal 0
         solveGold "+3 +3 +4 -2 -4" |> should equal 10
         solveGold "-6 +3 +8 +5 -6" |> should equal 5
         solveGold "+7 +7 -2 -7 -4" |> should equal 14

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem1.in")) |> should equal 423

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem1.in")) |> should equal 61126

     