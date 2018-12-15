﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Resources

[<EntryPoint>]
let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
    
    //res.GetString("problem1.in")      |> Problem1.solveSilver |> printfn "%d"
    //res.GetString("problem1.in")      |> Problem1.solveGold   |> printfn "%d"
    //res.GetString("problem2.in")      |> Problem2.solveSilver |> printfn "%d"
    //res.GetString("problem2.in")      |> Problem2.solveGold   |> printfn "%s"
    //res.GetString("problem3.in")      |> Problem3.solveSilver |> printfn "%d"
    //res.GetString("problem3.in")      |> Problem3.solveGold   |> printfn "%d"
    //res.GetString("problem4.in")      |> Problem4.solveSilver |> printfn "%d"
    //res.GetString("problem4.in")      |> Problem4.solveGold   |> printfn "%d"
    //res.GetString("problem5.in")      |> Problem5.solveSilver |> printfn "%d"
    //res.GetString("problem5.in")      |> Problem5.solveGold   |> printfn "%d"
    //res.GetString("problem5.in")      |> Problem5_ownLinkedListImplementation.solveSilver |> printfn "%d"
    //res.GetString("problem5.in")      |> Problem5_ownLinkedListImplementation.solveGold   |> printfn "%d"
    //res.GetString("problem6.in")      |> Problem6.solveSilver |> printfn "%d"
    //res.GetString("problem6.in")      |> Problem6.solveGold 10000 |> printfn "%d"
    //res.GetString("problem7.in")      |> Problem7.solveSilver |> printfn "%s"
    //res.GetString("problem7.in")      |> Problem7.solveGold 60 5 |> printfn "%d"
    //res.GetString("problem8.in")      |> Problem8.solveSilver |> printfn "%d"
    //res.GetString("problem8.in")      |> Problem8.solveGold   |> printfn "%d"
    //Problem9.solveSilver 418 71339L   |> printfn "%d"
    //Problem9.solveGold   418 71339L   |> printfn "%d"
    //res.GetString("problem10.in")     |> Problem10.solveGold   |> printfn "%d"
    //Problem11.solveSilver 9221        ||> printfn "%d,%d"
    //Problem11.solveGold 9221          |||> printfn "%d,%d,%d"
    //res.GetString("problem12.in")     |> Problem12.solveSilver "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####" |> printfn "%d"
    //res.GetString("problem12.in")     |> Problem12.solveGold   "##.##.##..#..#.#.#.#...#...#####.###...#####.##..#####.#..#.##..#..#.#...#...##.##...#.##......####" |> printfn "%d"
    //res.GetString("problem13.in")     |> Problem13.solveSilver ||> printfn "%d,%d"
    //res.GetString("problem13.in")     |> Problem13.solveGold   ||> printfn "%d,%d"
    //Problem14.solveSilver 825401      |> printfn "%s"
    //Problem14.solveGold 6 825401      |> printfn "%d"
    //res.GetString("problem15.in")      |> Problem15.solveSilver 32 32 |> printfn "%d"
    //res.GetString("problem15.in")      |> Problem15.solveGold 32 32   |> printfn "%d"
    res.GetString("problem16.in")      |> Problem16.solveSilver |> printfn "%d"
    res.GetString("problem16.in")      |> Problem16.solveGold   |> printfn "%d"

    //res.GetString("problemInfi.in")    |> ProblemInfi.solveSilver 20 20 |> printfn "%d"
    //res.GetString("problemInfi.in")    |> ProblemInfi.solveGold 20 20   |> printfn "%d"

    stopWatch.Stop()
    printfn ""
    printfn "Elapsed: %f ms" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
