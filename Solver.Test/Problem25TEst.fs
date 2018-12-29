namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem25
open System.Resources

type Problem25Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
        solveSilver ( "-1,2,2,0\n"
                    + "0,0,2,-2\n"
                    + "0,0,0,-2\n"
                    + "-1,2,0,0\n"
                    + "-2,-2,-2,2\n"
                    + "3,0,2,-1\n"
                    + "-1,3,2,2\n"
                    + "-1,0,-1,0\n"
                    + "0,2,1,-2\n"
                    + "3,0,0,0") |> should equal 4
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem25.in")) |> should equal 430
    
