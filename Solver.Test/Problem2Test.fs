namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem2
open System.Resources

type Problem2Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver (  "abcdef\n"
                      + "bababc\n"
                      + "abbcde\n"
                      + "abcccd\n"
                      + "aabcdd\n"
                      + "abcdee\n"
                      + "ababab" ) |> should equal 12

    [<Fact>]
    member x.solveGold_works () = 
         solveGold (    "abcde\n"
                      + "fghij\n"
                      + "klmno\n"
                      + "pqrst\n"
                      + "fguij\n"
                      + "axcye\n"
                      + "wvxyz" ) |> should equal "fgij"

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem2.in")) |> should equal 3952

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem2.in")) |> should equal "vtnikorkulbfejvyznqgdxpaw"

     