namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem6
open System.Resources

type Problem6Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "1, 1\n"
                        + "1, 6\n"
                        + "8, 3\n"
                        + "3, 4\n"
                        + "5, 5\n"
                        + "8, 9"    ) |> should equal 17

    [<Fact>]
    member x.solveGold_works () = 
         solveGold 32 (   "1, 1\n"
                        + "1, 6\n"
                        + "8, 3\n"
                        + "3, 4\n"
                        + "5, 5\n"
                        + "8, 9"    ) |> should equal 16

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem6.in")) |> should equal 5035

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 10000 (res.GetString("problem6.in")) |> should equal 35294

     