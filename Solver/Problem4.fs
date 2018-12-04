module Problem4

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    let parseInput input = 
        let sc = Scanner(input, false)
        let lines = sc.Lines |> Seq.sort |> Seq.toArray
        let guards = Dictionary<int, (int*int) list>() // ID -> nap from minute to minute
        let mutable currentGuard = -1
        let mutable startOfNap = -1
        for line in lines do
            match line with
            | Regex "^\[(?<year>\d+)-(?<month>\d+)-(?<day>\d+) (?<hour>\d+):(?<minute>\d+)\] Guard #(?<guard>\d+) begins shift$" [ year; month; day; hour; minute; guard ] ->
                currentGuard <- int guard
            | Regex "^\[(?<year>\d+)-(?<month>\d+)-(?<day>\d+) (?<hour>\d+):(?<minute>\d+)\] falls asleep$" [ year; month; day; hour; minute ] ->
                startOfNap <- int minute
            | Regex "^\[(?<year>\d+)-(?<month>\d+)-(?<day>\d+) (?<hour>\d+):(?<minute>\d+)\] wakes up$" [ year; month; day; hour; minute ] ->
                let endOfNap = int minute
                if not(guards.ContainsKey(currentGuard)) then guards.[currentGuard] <- []
                guards.[currentGuard] <- (startOfNap, endOfNap) :: guards.[currentGuard]
            | _ -> invalidOp "invalid line"
        guards

    let getMostSleepingMinute naps =
        let cnt = Array.create 60 0
        naps |> List.iter (fun (s,e) -> for i in s..e-1 do cnt.[i] <- cnt.[i] + 1)
        cnt |> Array.indexed |> Array.maxBy snd

    let solveSilver input = 
        let guards = parseInput input
        let mostSleepingGuard = guards |> Seq.maxBy (fun kvp -> kvp.Value |> List.sumBy (fun (s,e) -> e-s))
        let mostSleepingMinute = getMostSleepingMinute (mostSleepingGuard.Value)
        (mostSleepingGuard.Key) * (fst mostSleepingMinute)

    let solveGold input = 
        let guards = parseInput input
        let mostSleepingGuard = guards |> Seq.maxBy (fun kvp -> kvp.Value |> getMostSleepingMinute |> snd)
        let mostSleepingMinute = getMostSleepingMinute (mostSleepingGuard.Value)
        (mostSleepingGuard.Key) * (fst mostSleepingMinute)