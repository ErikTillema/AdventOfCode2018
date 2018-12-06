namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem5_ownLinkedListImplementation
open System.Resources

type Problem5_ownLinkedListImplementationTest() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "aA" |> should equal 0
         solveSilver "abB" |> should equal 1
         solveSilver "bBc" |> should equal 1
         solveSilver "abBc" |> should equal 2
         solveSilver "abBA" |> should equal 0
         solveSilver "FeabBCcdDAEf" |> should equal 0
         solveSilver "dabAcCaCBAcCcaDA" |> should equal 10

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "dabAcCaCBAcCcaDA" |> should equal 4

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem5.in")) |> should equal 9060

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem5.in")) |> should equal 6310

     