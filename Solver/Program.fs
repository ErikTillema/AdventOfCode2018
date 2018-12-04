// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Resources

[<EntryPoint>]
let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
    
    //res.GetString("problem1.in")                                |> Problem1.solveSilver |> printfn "%d"
    //res.GetString("problem1.in")                                |> Problem1.solveGold |> printfn "%d"
    //res.GetString("problem2.in")                                |> Problem2.solveSilver |> printfn "%d"
    //res.GetString("problem2.in")                                |> Problem2.solveGold |> printfn "%s"
    //res.GetString("problem3.in")                                |> Problem3.solveSilver |> printfn "%d"
    //res.GetString("problem3.in")                                |> Problem3.solveGold |> printfn "%d"
    //res.GetString("problem4.in")                                |> Problem4.solveSilver |> printfn "%d"
    //res.GetString("problem4.in")                                |> Problem4.solveGold |> printfn "%d"
    res.GetString("problem5.in")                                |> Problem5.solveSilver |> printfn "%d"
    res.GetString("problem5.in")                                |> Problem5.solveGold |> printfn "%d"

    //res.GetString("problemInfi.in")                             |> ProblemInfi.solveSilver 20 20 |> printfn "%d"
    //res.GetString("problemInfi.in")                             |> ProblemInfi.solveGold 20 20 |> printfn "%d"

    stopWatch.Stop()
    printfn ""
    printfn "Elapsed: %f ms" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
