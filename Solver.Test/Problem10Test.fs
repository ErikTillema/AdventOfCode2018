namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem10
open System.Resources

type Problem10Test() = 
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold (  "position=< 9,  1> velocity=< 0,  2>\n"
                    + "position=< 7,  0> velocity=<-1,  0>\n"
                    + "position=< 3, -2> velocity=<-1,  1>\n"
                    + "position=< 6, 10> velocity=<-2, -1>\n"
                    + "position=< 2, -4> velocity=< 2,  2>\n"
                    + "position=<-6, 10> velocity=< 2, -2>\n"
                    + "position=< 1,  8> velocity=< 1, -1>\n"
                    + "position=< 1,  7> velocity=< 1,  0>\n"
                    + "position=<-3, 11> velocity=< 1, -2>\n"
                    + "position=< 7,  6> velocity=<-1, -1>\n"
                    + "position=<-2,  3> velocity=< 1,  0>\n"
                    + "position=<-4,  3> velocity=< 2,  0>\n"
                    + "position=<10, -3> velocity=<-1,  1>\n"
                    + "position=< 5, 11> velocity=< 1, -2>\n"
                    + "position=< 4,  7> velocity=< 0, -1>\n"
                    + "position=< 8, -2> velocity=< 0,  1>\n"
                    + "position=<15,  0> velocity=<-2,  0>\n"
                    + "position=< 1,  6> velocity=< 1,  0>\n"
                    + "position=< 8,  9> velocity=< 0, -1>\n"
                    + "position=< 3,  3> velocity=<-1,  1>\n"
                    + "position=< 0,  5> velocity=< 0, -1>\n"
                    + "position=<-2,  2> velocity=< 2,  0>\n"
                    + "position=< 5, -2> velocity=< 1,  2>\n"
                    + "position=< 1,  4> velocity=< 2,  1>\n"
                    + "position=<-2,  7> velocity=< 2, -2>\n"
                    + "position=< 3,  6> velocity=<-1, -1>\n"
                    + "position=< 5,  0> velocity=< 1,  0>\n"
                    + "position=<-6,  0> velocity=< 2,  0>\n"
                    + "position=< 5,  9> velocity=< 1, -2>\n"
                    + "position=<14,  7> velocity=<-2,  0>\n"
                    + "position=<-3,  6> velocity=< 2, -1>\n") |> should equal 3

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem10.in")) |> should equal 10101

     