namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem23
open System.Resources

type Problem23Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
        solveSilver ( "pos=<0,0,0>, r=4\n"
                    + "pos=<1,0,0>, r=1\n"
                    + "pos=<4,0,0>, r=3\n"
                    + "pos=<0,2,0>, r=1\n"
                    + "pos=<0,5,0>, r=3\n"
                    + "pos=<0,0,3>, r=1\n"
                    + "pos=<1,1,1>, r=1\n"
                    + "pos=<1,1,2>, r=1\n"
                    + "pos=<1,3,1>, r=1") |> should equal 7
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem23.in")) |> should equal 674
    
    [<Fact>]
    member x.solveGold_works () = 
        solveGold   ( "pos=<10,12,12>, r=2\n"
                    + "pos=<12,14,12>, r=2\n"
                    + "pos=<16,12,12>, r=4\n"
                    + "pos=<14,14,14>, r=6\n"
                    + "pos=<50,50,50>, r=200\n"
                    + "pos=<10,10,10>, r=5") |> should equal 36

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem23.in")) |> should equal 129444177
    
