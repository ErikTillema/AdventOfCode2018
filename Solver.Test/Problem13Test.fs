namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem13
open System.Resources

type Problem13Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (    @"/->-\        " + "\n"
                        + @"|   |  /----\" + "\n"
                        + @"| /-+--+-\  |" + "\n"
                        + @"| | |  | v  |" + "\n"
                        + @"\-+-/  \-+--/" + "\n"
                        + @"  \------/   "         ) |> should equal (7,3)
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem13.in")) |> should equal (41,17)
    
    [<Fact>]
    member x.solveGold_works () = 
         solveGold   (    @"  />-<\" + "\n"
                        + @"|   |  " + "\n"
                        + @"| /<+-\" + "\n"
                        + @"| | | v" + "\n"
                        + @"\>+</ |" + "\n"
                        + @"  |   ^" + "\n"
                        + @"  \<->/"        ) |> should equal (6,4)
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem13.in")) |> should equal (134,117)
    
