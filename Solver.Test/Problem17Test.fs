namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem17
open System.Resources

type Problem17Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   (  "x=495, y=2..7\n"
                        + "y=7, x=495..501\n"
                        + "x=501, y=3..7\n"
                        + "x=498, y=2..4\n"
                        + "x=506, y=1..2\n"
                        + "x=498, y=10..13\n"
                        + "x=504, y=10..13\n"
                        + "y=13, x=498..504") |> should equal 57
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver  (res.GetString("problem17.in")) |> should equal 39557
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold     (  "x=495, y=2..7\n"
                        + "y=7, x=495..501\n"
                        + "x=501, y=3..7\n"
                        + "x=498, y=2..4\n"
                        + "x=506, y=1..2\n"
                        + "x=498, y=10..13\n"
                        + "x=504, y=10..13\n"
                        + "y=13, x=498..504") |> should equal 29
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold  (res.GetString("problem17.in")) |> should equal 32984
    
