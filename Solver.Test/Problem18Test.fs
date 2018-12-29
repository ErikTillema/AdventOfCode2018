namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem18
open System.Resources

type Problem18Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 10 10 (  ".#.#...|#.\n"
                            + ".....#|##|\n"
                            + ".|..|...#.\n"
                            + "..|#.....#\n"
                            + "#.#|||#|#|\n"
                            + "...#.||...\n"
                            + ".|....|...\n"
                            + "||...#|.#|\n"
                            + "|.||||..|.\n"
                            + "...#.|..|.") |> should equal 1147
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 50 50 (res.GetString("problem18.in")) |> should equal 737800
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 50 50 (res.GetString("problem18.in")) |> should equal 212040
    
