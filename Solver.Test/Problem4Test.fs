namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem4
open System.Resources

type Problem4Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    "[1518-11-01 00:00] Guard #10 begins shift\n"
                        + "[1518-11-01 00:05] falls asleep\n"
                        + "[1518-11-01 00:25] wakes up\n"
                        + "[1518-11-01 00:30] falls asleep\n"
                        + "[1518-11-01 00:55] wakes up\n"
                        + "[1518-11-01 23:58] Guard #99 begins shift\n"
                        + "[1518-11-02 00:40] falls asleep\n"
                        + "[1518-11-02 00:50] wakes up\n"
                        + "[1518-11-03 00:05] Guard #10 begins shift\n"
                        + "[1518-11-03 00:24] falls asleep\n"
                        + "[1518-11-03 00:29] wakes up\n"
                        + "[1518-11-04 00:02] Guard #99 begins shift\n"
                        + "[1518-11-04 00:36] falls asleep\n"
                        + "[1518-11-04 00:46] wakes up\n"
                        + "[1518-11-05 00:03] Guard #99 begins shift\n"
                        + "[1518-11-05 00:45] falls asleep\n"
                        + "[1518-11-05 00:55] wakes up" ) |> should equal 240

    [<Fact>]
    member x.solveGold_works () = 
         solveGold  (     "[1518-11-01 00:00] Guard #10 begins shift\n"
                        + "[1518-11-01 00:05] falls asleep\n"
                        + "[1518-11-01 00:25] wakes up\n"
                        + "[1518-11-01 00:30] falls asleep\n"
                        + "[1518-11-01 00:55] wakes up\n"
                        + "[1518-11-01 23:58] Guard #99 begins shift\n"
                        + "[1518-11-02 00:40] falls asleep\n"
                        + "[1518-11-02 00:50] wakes up\n"
                        + "[1518-11-03 00:05] Guard #10 begins shift\n"
                        + "[1518-11-03 00:24] falls asleep\n"
                        + "[1518-11-03 00:29] wakes up\n"
                        + "[1518-11-04 00:02] Guard #99 begins shift\n"
                        + "[1518-11-04 00:36] falls asleep\n"
                        + "[1518-11-04 00:46] wakes up\n"
                        + "[1518-11-05 00:03] Guard #99 begins shift\n"
                        + "[1518-11-05 00:45] falls asleep\n"
                        + "[1518-11-05 00:55] wakes up" ) |> should equal 4455

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem4.in")) |> should equal 14346

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem4.in")) |> should equal 5705

     