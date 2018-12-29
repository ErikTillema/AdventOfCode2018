namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem16
open System.Resources

type Problem16Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver    (  "Before: [3, 2, 1, 1]\n"
                         + "9 2 1 2\n"
                         + "After:  [3, 2, 2, 1]\n") |> should equal 1
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver  (res.GetString("problem16.in")) |> should equal 592
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem16.in")) (res.GetString("problem16part2.in")) |> should equal 557
    
