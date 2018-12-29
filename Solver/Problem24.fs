module Problem24

    open System
    open Util
    open SeqExt
    open System.Collections.Generic

    type Side = ImmuneSystem | Infection
    type AttackType = Fire | Slashing | Cold | Bludgeoning | Radiation
    type Weakness = Weakness of AttackType
    type Immunity = Immunity of AttackType
    type Group = { side: Side;
                   mutable unitCount: int;
                   hitPoints: int;
                   attack: int;
                   attackType: AttackType; 
                   initiative: int; // serves as identifier as well, unique per Group.
                   weaknesses: Weakness list;
                   immunities: Immunity list; }
    type Outcome = Tie | Winner of Side

    let effectivePower group = group.unitCount * group.attack
    let isAlive group = group.unitCount > 0

    let getInitialGroups boost isTest = 
        if isTest then
            List<Group>([
                { side = ImmuneSystem; unitCount = 17; hitPoints = 5390;  weaknesses = [Weakness(Radiation); Weakness(Bludgeoning)]; immunities = []; attack = 4507+boost; attackType = Fire; initiative = 2; };
                { side = ImmuneSystem; unitCount = 989; hitPoints = 1274;  weaknesses = [Weakness(Bludgeoning); Weakness(Slashing)]; immunities = [Immunity(Fire)]; attack = 25+boost; attackType = Slashing; initiative = 3; };
                { side = Infection;    unitCount = 801; hitPoints = 4706;  weaknesses = [Weakness(Radiation)]; immunities = []; attack = 116; attackType = Bludgeoning; initiative = 1; };
                { side = Infection;    unitCount = 4485; hitPoints = 2961;  weaknesses = [Weakness(Cold); Weakness(Fire)]; immunities = [Immunity(Radiation)]; attack = 12; attackType = Slashing; initiative = 4; };
            ])
        else
            List<Group>([  
                { side = ImmuneSystem; unitCount = 698; hitPoints = 10286;  weaknesses = []; immunities = []; attack = 133+boost; attackType = Fire; initiative = 9; };
                { side = ImmuneSystem; unitCount = 6846; hitPoints = 2773;  weaknesses = [Weakness(Slashing); Weakness(Cold)]; immunities = []; attack = 4+boost; attackType = Slashing; initiative = 14; };
                { side = ImmuneSystem; unitCount = 105; hitPoints = 6988;   weaknesses = [Weakness(Bludgeoning)]; immunities = [Immunity(Radiation)]; attack = 616+boost; attackType = Radiation; initiative = 17; };
                { side = ImmuneSystem; unitCount = 5615; hitPoints = 7914;  weaknesses = [Weakness(Bludgeoning)]; immunities = []; attack = 13+boost; attackType = Radiation; initiative = 20; };
                { side = ImmuneSystem; unitCount = 1021; hitPoints = 10433; weaknesses = [Weakness(Cold)]; immunities = [Immunity(Slashing); Immunity(Bludgeoning)]; attack = 86+boost; attackType = Bludgeoning; initiative = 12; };
                { side = ImmuneSystem; unitCount = 6099; hitPoints = 11578; weaknesses = []; immunities = []; attack = 15+boost; attackType = Bludgeoning; initiative = 13; };
                { side = ImmuneSystem; unitCount = 82; hitPoints = 1930;    weaknesses = [Weakness(Bludgeoning)]; immunities = [Immunity(Cold)]; attack = 179+boost; attackType = Bludgeoning; initiative = 5; };
                { side = ImmuneSystem; unitCount = 2223; hitPoints = 9442;  weaknesses = []; immunities = [Immunity(Bludgeoning)]; attack = 38+boost; attackType = Cold; initiative = 19; };
                { side = ImmuneSystem; unitCount = 140; hitPoints = 7594;   weaknesses = [Weakness(Radiation)]; immunities = []; attack = 452+boost; attackType = Fire; initiative = 8; };
                { side = ImmuneSystem; unitCount = 3057; hitPoints = 3871;  weaknesses = [Weakness(Bludgeoning)]; immunities = []; attack = 11+boost; attackType = Radiation; initiative = 16; };
                { side = Infection;    unitCount = 263; hitPoints = 48098;  weaknesses = [Weakness(Slashing)]; immunities = [Immunity(Radiation)]; attack = 293; attackType = Bludgeoning; initiative = 2; };
                { side = Infection;    unitCount = 111; hitPoints = 9893;   weaknesses = []; immunities = [Immunity(Slashing)]; attack = 171; attackType = Fire; initiative = 18; };
                { side = Infection;    unitCount = 2790; hitPoints = 36205; weaknesses = []; immunities = []; attack = 25; attackType = Cold; initiative = 4; };
                { side = Infection;    unitCount = 3325; hitPoints = 46479; weaknesses = [Weakness(Slashing)]; immunities = []; attack = 27; attackType = Radiation; initiative = 1; };
                { side = Infection;    unitCount = 3593; hitPoints = 6461;  weaknesses = [Weakness(Fire); Weakness(Slashing)]; immunities = []; attack = 3; attackType = Radiation; initiative = 15; };
                { side = Infection;    unitCount = 2925; hitPoints = 13553; weaknesses = [Weakness(Cold); Weakness(Bludgeoning)]; immunities = [Immunity(Fire)]; attack = 8; attackType = Cold; initiative = 10; };
                { side = Infection;    unitCount = 262; hitPoints = 43260;  weaknesses = [Weakness(Cold)]; immunities = []; attack = 327; attackType = Radiation; initiative = 6; };
                { side = Infection;    unitCount = 4228; hitPoints = 24924; weaknesses = [Weakness(Radiation); Weakness(Fire)]; immunities = [Immunity(Cold); Immunity(Bludgeoning)]; attack = 11; attackType = Cold; initiative = 11; };
                { side = Infection;    unitCount = 689; hitPoints = 42315;  weaknesses = [Weakness(Cold); Weakness(Slashing)]; immunities = []; attack = 116; attackType = Fire; initiative = 7; };
                { side = Infection;    unitCount = 2649; hitPoints = 37977; weaknesses = [Weakness(Radiation)]; immunities = []; attack = 24; attackType = Cold; initiative = 3; }
            ])

    let getDamage groupFrom groupToo = 
        let baseDamage = effectivePower groupFrom
        let factor =
            if groupToo.immunities |> List.contains (Immunity(groupFrom.attackType)) then 0
            elif groupToo.weaknesses |> List.contains (Weakness(groupFrom.attackType)) then 2
            else 1
        baseDamage * factor

    let rec doFights groups = 
        // returns total number of kills in this fight.
        let doFight() =
            // target selection
            let target = Dictionary<int,Group>() // initiative -> Group, use initiative as identifier.
            let targets = HashSet<Group>()
            let mutable totalKills = 0
            for group in groups |> Seq.filter isAlive |> Seq.sortByDescending (fun g -> (effectivePower g, g.initiative)) do
                let t = groups  |> Seq.filter isAlive
                                |> Seq.filter (fun g -> g.side <> group.side)
                                |> Seq.filter (fun g -> not(targets.Contains(g)))
                                |> Seq.filter (fun g -> getDamage group g > 0)
                                |> Seq.sortByDescending (fun g -> (getDamage group g, effectivePower g, g.initiative))
                                |> Seq.tryHead
                match t with
                | Some(targetGroup) -> target.[group.initiative] <- targetGroup
                                       targets.Add(targetGroup) |> ignore
                | None -> ()

            // attack
            for group in groups |> Seq.sortByDescending (fun g -> g.initiative) do
                if isAlive group && target.ContainsKey(group.initiative) then // is still alive and has target
                    let t = target.[group.initiative]
                    let damage = getDamage group t
                    let kills = min t.unitCount (damage/t.hitPoints)
                    totalKills <- totalKills + kills
                    t.unitCount <- t.unitCount - kills
            
            totalKills

        if groups |> Seq.filter isAlive |> Seq.map (fun g -> g.side) |> SeqExt.allEqual then
            let winningSide = (groups |> Seq.filter isAlive |> Seq.head).side
            Winner(winningSide)
        else
            if doFight() = 0 then
                Tie
            else 
                doFights groups

    let getResult boost isTest = 
        let groups = getInitialGroups boost isTest
        doFights groups |> ignore
        groups |> Seq.filter isAlive |> Seq.sumBy (fun g -> g.unitCount)

    let solveSilver isTest = 
        getResult 0 isTest
        
    let solveGold isTest = 
        let rec bsearch start endd =
            let isOk boost =
                let groups = getInitialGroups boost isTest
                match doFights groups with
                | Winner(ImmuneSystem) -> true
                | _ -> false
            if start = endd then start
            else
                let mid = (start+endd)/2
                if isOk mid then bsearch start mid
                else bsearch (mid+1) endd
        
        let boost = bsearch 0 1000000
        getResult boost isTest

    