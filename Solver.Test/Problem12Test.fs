namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem12
open System.Resources

type Problem12Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "#..#.#..##......###...###" ( 
                              "...## => #\n"
                            + "..#.. => #\n"
                            + ".#... => #\n"
                            + ".#.#. => #\n"
                            + ".#.## => #\n"
                            + ".##.. => #\n"
                            + ".#### => #\n"
                            + "#.#.# => #\n"
                            + "#.### => #\n"
                            + "##.#. => #\n"
                            + "##.## => #\n"
                            + "###.. => #\n"
                            + "###.# => #\n"
                            + "####. => #") |> should equal 325
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####" (res.GetString("problem12.in")) |> should equal 2930

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####" (res.GetString("problem12.in")) |> should equal 3099999999491L

     