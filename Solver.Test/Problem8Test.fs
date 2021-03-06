﻿namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem8
open System.Resources

type Problem8Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" ) |> should equal 138

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     (  "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" ) |> should equal 66

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem8.in")) |> should equal 47244

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem8.in")) |> should equal 17267

     