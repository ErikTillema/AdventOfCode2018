﻿namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem15
open System.Resources

type Problem15Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 9 9 (  "#########\n"
                          + "#G......#\n"
                          + "#.E.#...#\n"
                          + "#..##..G#\n"
                          + "#...##..#\n"
                          + "#...#...#\n"
                          + "#.G...G.#\n"
                          + "#.....G.#\n"
                          + "#########") |> should equal 18740
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 32 32 (res.GetString("problem15.in")) |> should equal 229798
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold 7 7 (  "#######\n"
                        + "#.G...#\n"
                        + "#...EG#\n"
                        + "#.#.#G#\n"
                        + "#..G#E#\n"
                        + "#.....#\n"
                        + "#######") |> should equal 4988
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 32 32 (res.GetString("problem15.in")) |> should equal 52972
    
