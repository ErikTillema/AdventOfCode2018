namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem16
open System.Resources

type Problem16Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver     (  "#########\n"
                          + "#G......#\n"
                          + "#.E.#...#\n"
                          + "#..##..G#\n"
                          + "#...##..#\n"
                          + "#...#...#\n"
                          + "#.G...G.#\n"
                          + "#.....G.#\n"
                          + "#########") |> should equal 42
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver  (res.GetString("problem16.in")) |> should equal 42
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold     (  "#######\n"
                        + "#.G...#\n"
                        + "#...EG#\n"
                        + "#.#.#G#\n"
                        + "#..G#E#\n"
                        + "#.....#\n"
                        + "#######") |> should equal 42
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem16.in")) |> should equal 42
    
