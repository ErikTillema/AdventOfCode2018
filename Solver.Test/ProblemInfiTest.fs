namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open ProblemInfi
open System.Resources

type ProblemInfiTest() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 4 4 ( "╔═╗║\n" +
                           "╠╗╠║\n" + 
                           "╬╬╣╬\n" + 
                           "╚╩╩═" ) |> should equal 6

    [<Fact>]
    member x.solveGold_works () = 
         solveGold 4 4   ( "╔═╗║\n" +
                           "╠╗╠║\n" + 
                           "╬╬╣╬\n" + 
                           "╚╩╩═" ) |> should equal 4

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 20 20 (res.GetString("problemInfi.in")) |> should equal 42

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 20 20 (res.GetString("problemInfi.in")) |> should equal 44

     