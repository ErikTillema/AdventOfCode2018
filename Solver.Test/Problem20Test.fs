namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem20
open System.Resources

type Problem20Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "^WNE$" |> should equal 3
         solveSilver "^ENWWW(NEEE|SSE(EE|N))$" |> should equal 10
         solveSilver "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" |> should equal 18
         solveSilver "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" |> should equal 23
         solveSilver "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" |> should equal 31
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem20.in")) |> should equal 3879
    
    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem20.in")) |> should equal 8464
    
