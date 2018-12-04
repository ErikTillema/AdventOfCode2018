namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem5
open System.Resources

type Problem5Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "[1518-11-01 00:00] Guard #10 begins shift\n"
                        + "[1518-11-01 00:05] falls asleep\n"
                        + "[1518-11-05 00:55] wakes up" ) |> should equal 42

    [<Fact>]
    member x.solveGold_works () = 
         solveGold  (     "[1518-11-01 00:00] Guard #10 begins shift\n"
                        + "[1518-11-01 00:05] falls asleep\n"
                        + "[1518-11-05 00:55] wakes up" ) |> should equal 42

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem5.in")) |> should equal 42

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem5.in")) |> should equal 42

     