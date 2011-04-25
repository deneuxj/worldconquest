module AttackOrders

open System.Collections.Generic

open GameState
open Units
open HexTiling
open Orders

type AttackType =
    | Remote  // The attacker is safe
    | Melee   // One of the defenders (the most dangerous one) will strike back


type AttackOrder =
    {  player : PlayerId
       unit : UnitIndex
       coords : HexCoords
       attack : AttackType  }

let extractAttackOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getUnitOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
        let coords_and_attack =
            if
                match idx with
                | UnitIndex.Root _ -> true
                | UnitIndex.Transported (idx, _) ->
                    match u.specific, units.[idx].specific with
                    | AirUnit, Carrier _ -> true
                    | _ -> false
                | _ -> false
            then
                match order with
                | Order.Bomb (coords, _)
                | Order.Bombard coords -> Some (coords, Remote)
                | Order.DirectAttack (coords, _) -> Some (coords, Melee)
                | _ -> None
            else
                None

        match coords_and_attack with
        | Some (coords, attack) ->
            [|
                {  player = PlayerId player;
                    unit = idx;
                    coords = coords;
                    attack = attack  }
            |]
        | None -> Array.empty

    let fetchUnitOrder = mkFetchOrderMap getUnitOrder orders
    playerUnitMap fetchUnitOrder (fun _ -> true) units
    |> Array.concat


let computeDamages (gs : GameState) (player : int) (orders : AttackOrder[]) =

    let enemyUnits =
        [|
            for i in 0..gs.player_units.Length - 1 do
                let victim = PlayerId i
                if i <> player then
                    for i2 in 0..gs.player_units.[i].Length - 1 do
                        let u = gs.player_units.[i].[i2]
                        yield victim, i, u.coords
        |]
        |> Seq.groupBy (fun (victim, i, coords) -> coords)
        |> dict

    let damages_to_victims =
        [
            for attack in orders do
                match enemyUnits.TryGetValue(attack.coords) with
                | true, units ->                
                    let attacker = getUnitByIndex gs.player_units.[player] attack.unit
                    for (PlayerId victim_player) as vp_id, victim_idx, _ in units do
                        let victim_unit = gs.player_units.[victim_player].[victim_idx]
                        let bonus =
                            match attacker.specific, victim_unit.specific, attack.attack with
                            | Infantry, Infantry, Melee
                            | Tank, Tank, Melee
                            | Tank, Destroyer(Docked.Docked), Melee
                            | Tank, Battleship(Docked.Docked), Melee
                            | Artillery, Artillery, Melee
                            | Artillery, AntiAircraft, Melee
                            | Artillery, Battleship _, Remote
                            | AntiAircraft, Artillery, Melee
                            | AntiAircraft, AntiAircraft, Melee
                            | Destroyer _, Destroyer _, Melee
                            | Destroyer _, Battleship _, Melee
                            | Destroyer _, Carrier _, Melee
                            | Submarine _, Submarine _, Melee
                            | Battleship _, Battleship _, _
                            | Carrier _, Carrier _, Melee
                            | Fighter _, AntiAircraft, Melee
                            | Fighter _, Destroyer _, Melee
                            | Fighter _, Submarine _, Melee
                            | Fighter _, Battleship _, Melee
                            | Fighter _, Carrier _, Melee
                            | Fighter _, Fighter(Airborne, _), Melee
                            | Bomber _, Bomber(Airborne, _, _), Melee -> 0.0f

                            | BomberWithBombs _, Transport _, Remote -> +1.0f
                            | BomberWithBombs _, AirUnit, Remote -> failwith "Bomber can't bomb airborne units"
                            | BomberWithBombs _, _, Remote -> 0.0f

                            | Infantry, Tank, Melee
                            | Infantry, Submarine(Docked.Docked, _), Melee
                            | Infantry, Carrier(Docked.Docked, _), Melee
                            | Infantry, Fighter(Landed.Landed, _), Melee
                            | Infantry, Bomber(Landed.Landed, _, _), Melee
                            | Tank, Infantry, Melee
                            | Tank, Submarine(Docked.Docked, _), Melee
                            | Tank, Carrier(Docked.Docked, _), Melee
                            | Tank, Fighter(Landed.Landed, _), Melee
                            | Tank, Bomber(Landed.Landed, _, _), Melee
                            | Artillery, Transport(Docked.Docked, _), Melee
                            | Artillery, Carrier(Docked.Docked, _), _
                            | Artillery, Fighter(Landed.Landed, _), Melee
                            | Artillery, Bomber(Landed.Landed, _, _), Melee
                            | Artillery, Tank, Remote
                            | Artillery, Destroyer _, Remote
                            | Artillery, Carrier _, Remote
                            | AntiAircraft, Transport(Docked.Docked, _), Melee
                            | AntiAircraft, Submarine(Docked.Docked, _), Melee
                            | AntiAircraft, Carrier(Docked.Docked, _), Melee
                            | AntiAircraft, Fighter(Landed.Landed, _), Melee
                            | AntiAircraft, Bomber(Landed.Landed, _, _), Melee
                            | Destroyer _, Fighter(Airborne, _), Melee
                            | Destroyer _, Bomber(Airborne, _, _), Melee
                            | Submarine(_, _), Battleship _, Melee
                            | Submarine(_, _), Carrier _, Melee
                            | Battleship _, Submarine _, Melee
                            | Battleship _, Carrier _, Melee
                            | Battleship _, Tank, Remote
                            | Battleship _, Destroyer _, Remote
                            | Battleship _, Carrier _, Remote
                            | Carrier _, Transport _, Melee
                            | Fighter _, LandUnit, Melee
                            | Fighter _, Bomber(Airborne, _, _), Melee -> 1.0f

                            | Infantry, Artillery, Melee
                            | Infantry, AntiAircraft, Melee
                            | Infantry, Transport(Docked.Docked, _), Melee
                            | Tank, Artillery, Melee
                            | Tank, AntiAircraft, Melee
                            | Tank, Transport(Docked.Docked, _), Melee
                            | Artillery, LandUnit, Remote
                            | Artillery, Transport _ , Remote
                            | Artillery, Landed, Remote
                            | AntiAircraft, AirUnit, Remote
                            | _, Transport _, _
                            | Destroyer _, Submarine _, Melee
                            | Battleship _, Destroyer _, Melee
                            | Battleship _, _, Remote
                            | AirUnit, Landed, Melee
                            | Submarine _, Transport _, Melee -> 2.0f

                            | Artillery, Infantry, Melee
                            | Artillery, Tank, Melee
                            | Artillery, Destroyer(Docked.Docked), Melee
                            | Artillery, Battleship(Docked.Docked), Melee
                            | AntiAircraft, Infantry, Melee
                            | AntiAircraft, Tank, Melee
                            | AntiAircraft, Destroyer(Docked.Docked), Melee
                            | AntiAircraft, Battleship(Docked.Docked), Melee
                            | Submarine(_, NotStealthy), Destroyer _, Melee
                            | Carrier _, Destroyer _, Melee
                            | Carrier _, Submarine _, Melee
                            | Bomber _, AntiAircraft, Melee
                            | Bomber _, Fighter(Airborne, _), Melee
                            | Infantry, Destroyer(Docked.Docked), Melee
                            | Infantry, Battleship(Docked.Docked), Melee -> -1.0f

                            | Carrier _, Battleship _, Melee -> -2.0f

                            | _ -> failwith <| sprintf "Invalid attack %A from %A on %A" attack.attack attacker victim_unit

                        let damage = base_damage + bonus
                        let health_points = getHealthPoints victim_unit.specific
                        yield (vp_id, victim_idx, damage / health_points)

                | false, _ -> ()
        ]

    let damages_to_attackers =
        [
            for attack in orders do
                let relative_damage =
                    [
                        match enemyUnits.TryGetValue(attack.coords) with
                        | true, units ->                
                            let attacker = getUnitByIndex gs.player_units.[player] attack.unit
                            let health_points = getHealthPoints attacker.specific
                            for (PlayerId victim_player) as vp_id, victim_idx, _ in units do
                                let victim_unit = gs.player_units.[victim_player].[victim_idx]
                                yield
                                    match attacker.specific, victim_unit.specific, attack.attack with
                                    // Catch some invalid cases (invalid attacks)
                                    | Transport _, _, _ -> failwith "Transports cannot attack"
                                    | SeaUnit, LandUnit, Melee
                                    | Docked, LandUnit, Melee -> failwith "Naval units cannot melee-attack land units."
                                    | LandUnit, SeaUnit, Melee -> failwith "Land units cannot melee-attack naval units at sea."
                                    | AntiAircraft, LandUnit, Remote -> failwith "Anti-aircrafts cannot bombard land units."
                                    | AntiAircraft, Landed, Remote -> failwith "Anti-aircrafts cannot bombard aircrafts on the ground."
                                    | AntiAircraft, SeaUnit, Remote
                                    | AntiAircraft, Docked, Remote -> failwith "Anti-aircrafts cannot bombard naval units."

                                    // The victim cannot fire back
                                    | _, _, Remote
                                    | _, Transport _, _
                                    | _, Landed, _ -> 0.0f
                            
                                    // The victim unit inflicts extra damage
                                    | Tank, Destroyer(Docked.Docked), Melee
                                    | Tank, Battleship(Docked.Docked), Melee
                                    | Artillery, Destroyer(Docked.Docked), Melee
                                    | Artillery, Battleship(Docked.Docked), Melee
                                    | AntiAircraft, Destroyer(Docked.Docked), Melee
                                    | AntiAircraft, Battleship(Docked.Docked), Melee
                                    | Submarine(_, Stealthy.NotStealthy), Destroyer _, Melee
                                    | Carrier _, SeaUnit, Melee
                                    | Carrier _, Docked, Melee
                                    | Fighter _, AntiAircraft, Melee
                                    | Bomber _, AntiAircraft, Melee
                                    | Bomber _, Fighter(Landed.Airborne, _), Melee -> (base_damage + 1.0f) / health_points

                                    // Default case: normal damage
                                    | _, _, _ -> base_damage / health_points

                        | false, _ -> yield 0.0f
                    ]
                    |> List.max

                if relative_damage > 0.0f then
                    yield (PlayerId player, attack.unit, relative_damage)
        ]

    damages_to_victims, damages_to_attackers


let accumulateDamage (damages_to_victims, damages_to_attackers) =
    let damages_to_victims =
        damages_to_victims
        |> List.map (fun (id : PlayerId, idx, damage) -> (id, Root idx, damage))

    Seq.concat [damages_to_victims; damages_to_attackers]
    |> Seq.groupBy (fun (id, idx, _) -> (id, idx))
    |> Seq.map (fun ((id, idx), damages) -> ((id, idx), Seq.sumBy (fun (_, _, damage : float32) -> damage) damages))
    |> dict


let getUnitDeaths (gs : GameState) (accumulated_damages : IDictionary<PlayerId * UnitIndex, float32>) =
    accumulated_damages
    |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
    |> Seq.filter (fun ((PlayerId id, idx), damage) -> (getUnitByIndex (gs.player_units.[id]) idx).health <= damage)
    |> Seq.map fst
    |> Set.ofSeq
