namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem19
open System.Resources

type Problem19Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 0 (  "#ip 0\n"
                        + "seti 5 0 1\n"
                        + "seti 6 0 2\n"
                        + "addi 0 1 0\n"
                        + "addr 1 2 3\n"
                        + "setr 1 0 0\n"
                        + "seti 8 0 4\n"
                        + "seti 9 0 5\n") |> should equal 7
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 2 (res.GetString("problem19.in")) |> should equal 2280
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 2 (res.GetString("problem19.in")) |> should equal 30481920
    
