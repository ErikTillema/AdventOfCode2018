﻿namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem7
open System.Resources

type Problem7Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "Step C must be finished before step A can begin.\n"
                        + "Step C must be finished before step F can begin.\n"
                        + "Step A must be finished before step B can begin.\n"
                        + "Step A must be finished before step D can begin.\n"
                        + "Step B must be finished before step E can begin.\n"
                        + "Step D must be finished before step E can begin.\n"
                        + "Step F must be finished before step E can begin." ) |> should equal "CABDFE"

    [<Fact>]
    member x.solveGold_works () = 
         solveGold 0 2 (  "Step C must be finished before step A can begin.\n"
                        + "Step C must be finished before step F can begin.\n"
                        + "Step A must be finished before step B can begin.\n"
                        + "Step A must be finished before step D can begin.\n"
                        + "Step B must be finished before step E can begin.\n"
                        + "Step D must be finished before step E can begin.\n"
                        + "Step F must be finished before step E can begin." ) |> should equal 15

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem7.in")) |> should equal "EUGJKYFQSCLTWXNIZMAPVORDBH"

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 60 5 (res.GetString("problem7.in")) |> should equal 1014

     