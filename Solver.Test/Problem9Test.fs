namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem9
open System.Resources

type Problem9Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" ) |> should equal 42

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     (  "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" ) |> should equal 42

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem9.in")) |> should equal 42

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem9.in")) |> should equal 42

     