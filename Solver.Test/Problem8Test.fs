namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem8
open System.Resources

type Problem8Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "Step C must be finished before step A can begin.\n"
                        + "Step C must be finished before step F can begin.\n"
                        + "Step A must be finished before step B can begin.\n"
                        + "Step A must be finished before step D can begin.\n"
                        + "Step B must be finished before step E can begin.\n"
                        + "Step D must be finished before step E can begin.\n"
                        + "Step F must be finished before step E can begin." ) |> should equal 42

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     (  "Step C must be finished before step A can begin.\n"
                        + "Step C must be finished before step F can begin.\n"
                        + "Step A must be finished before step B can begin.\n"
                        + "Step A must be finished before step D can begin.\n"
                        + "Step B must be finished before step E can begin.\n"
                        + "Step D must be finished before step E can begin.\n"
                        + "Step F must be finished before step E can begin." ) |> should equal 42

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem8.in")) |> should equal 42

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem8.in")) |> should equal 42

     