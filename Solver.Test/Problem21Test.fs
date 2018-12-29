namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem21

type Problem21Test() = 
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver() |> should equal 7129803L
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold() |> should equal 12284643L
    
