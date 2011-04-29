#r @"bin\Debug\XNAUtils.dll"

#load "PathFinding.fs"
#load "HexTiling.fs"
#load "Terrain.fs"
#load "Resource.fs"
#load "Units.fs"
#load "GameState.fs"
#load "Orders.fs"
#load "AttackOrders.fs"
#load "MoveOrders.fs"
#load "EmbarkOrders.fs"
#load "GameStateUpdate.fs"

#load "Ai.fs"


let width = 32

let terr =
    let inMid x = x >= width / 4 && x < 3 * width / 4
    Array2D.init width width (fun i j -> if inMid i && inMid j then Terrain.Land else Terrain.Sea)

let default_gs : GameState.GameState =
    { terrain = terr ;
        resource_at = [] |> dict ;
        resources_of = [||] ;
        player_units = [||] }

let ofSq (i, j) =
    HexTiling.SquareCoords(i, j)
    |> HexTiling.toHex

let findPath start goal =    
    PathFinding.find
        (HexTiling.distWrap width goal >> float32)
        (HexTiling.neighboursOf)
        (fun _ -> 1.0f)
        start
        (System.Single.PositiveInfinity)

let assertTrue out txt = function
    | true -> out <| sprintf "PASSED: %s" txt
    | false -> out <| sprintf "FAILED: %s" txt

let zeroTest() =
    let gs = default_gs
    let gs' = GameStateUpdate.update gs [||]

    assertTrue (printfn "%s") "ZERO" (gs = gs')

let moveTest() =
    let start = ofSq(width / 2, width / 2)
    let goal = ofSq(width / 2 - 1, width / 2)

    let units_and_orders : (Units.UnitInfo * _)[] =
        [|
            { coords = start ;
                health = 1.0f ;
                moves = Units.tank_range ;
                specific = Units.UnitTypes.Tank },
            Orders.Move ((findPath start goal).Value)
        |]

    let gs =
        { default_gs with
            player_units = [| Array.unzip units_and_orders |> fst |]
        }

    let gs' = GameStateUpdate.update gs [| Array.unzip units_and_orders |> snd |]

    match gs'.player_units with
    | [| [| { coords = x } |] |] when x = goal -> true
    | _ -> false

    |> assertTrue (printfn "%s") "MOVE"

let bombardTest() =
    let u0 = ofSq(width / 2, width / 2)
    let u1 = ofSq(width / 2 + 1, width / 2 + 1)

    let p0 : (Units.UnitInfo * _)[] =
        [|
            { coords = u0 ;
                health = 1.0f ;
                moves = Units.artillery_range ;
                specific = Units.UnitTypes.Artillery },
            Orders.Bombard u1
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
                moves = Units.infantry_range ;
                specific = Units.UnitTypes.Infantry },
            Orders.DoNothing
        |]

    let gs =
       { default_gs with
            player_units =
                [|
                    Array.unzip p0 |> fst ;
                    Array.unzip p1 |> fst
                |]
        }

    let gs' =
        GameStateUpdate.update gs
            [|
                Array.unzip p0 |> snd ;
                Array.unzip p1 |> snd
            |]

    match gs'.player_units with
    | [| _; [| { health = x } |] |] when x < 1.0f -> true // Injured
    | [| _; [||] |] -> true  // Killed
    | _ -> false

    |> assertTrue (printfn "%s") "BOMBARD"


let attackTest() =
    let u0 = ofSq(width / 2, width / 2)
    let u1 = ofSq(width / 2 + 1, width / 2 + 1)

    let p0 : (Units.UnitInfo * _)[] =
        [|
            { coords = u0 ;
                health = 1.0f ;
                moves = Units.tank_range ;
                specific = Units.UnitTypes.Tank },
            Orders.DirectAttack(u1, [])
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
                moves = Units.infantry_range ;
                specific = Units.UnitTypes.Infantry },
            Orders.DoNothing
        |]

    let gs =
       { default_gs with
            player_units =
                [|
                    Array.unzip p0 |> fst ;
                    Array.unzip p1 |> fst
                |]
        }

    printfn "%A" gs

    let gs' =
        GameStateUpdate.update gs
            [|
                Array.unzip p0 |> snd ;
                Array.unzip p1 |> snd
            |]

    printfn "%A" gs'

    match gs'.player_units with
    | [| _; [| { health = x } |] |] when x < 1.0f -> true // Injured
    | [| _; [||] |] -> true  // Killed
    | _ -> false

    |> assertTrue (printfn "%s") "DIRECT ATTACK"


let stackAttackTest() =
    let u0 = ofSq(width / 2, width / 2)
    let u1 = ofSq(width / 2 + 1, width / 2 + 1)

    let p0 : (Units.UnitInfo * _)[] =
        [|
            { coords = u0 ;
                health = 1.0f ;
                moves = Units.tank_range ;
                specific = Units.UnitTypes.Tank },
            Orders.DirectAttack(u1, [])
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
                moves = Units.infantry_range ;
                specific = Units.UnitTypes.Infantry },
            Orders.DoNothing ;

            { coords = u1 ;
                health = 1.0f ;
                moves = Units.infantry_range ;
                specific = Units.UnitTypes.Infantry },
            Orders.DoNothing ;
        |]

    let gs =
       { default_gs with
            player_units =
                [|
                    Array.unzip p0 |> fst ;
                    Array.unzip p1 |> fst
                |]
        }

    printfn "%A" gs

    let gs' =
        GameStateUpdate.update gs
            [|
                Array.unzip p0 |> snd ;
                Array.unzip p1 |> snd
            |]

    printfn "%A" gs'

    match gs'.player_units with
    | [| [| _ |]; [| { health = x1 } ; { health = x2 } |] |] when x1 < 1.0f && x2 < 1.0f -> true // Injured
    | [| [| _ |]; [| { health = x1 } |] |] when x1 < 1.0f -> true // 1 injured, 1 killed
    | [| [| _ |]; [||] |] -> true  // All killed
    | _ -> false

    |> assertTrue (printfn "%s") "DIRECT ATTACK OF STACK"


let randomTest() =
    let rnd = new System.Random()

    let rndTransport docked coords : Units.UnitInfo =
        let transported_units =
            [|
                while rnd.Next(4) <= 2 do
                    yield match rnd.Next(4) with
                          | 0 -> Units.TransportedUnit.AntiAircraft(Units.Health 1.0f)
                          | 1 -> Units.TransportedUnit.Artillery(Units.Health 1.0f)
                          | 2 -> Units.TransportedUnit.Infantry(Units.Health 1.0f)
                          | _ -> Units.TransportedUnit.Tank(Units.Health 1.0f)
            |]

        { coords = coords ;
          moves = Units.transport_range ;
          health = 1.0f ;
          specific = Units.Transport(docked, transported_units)
        }

    let rndBomberLifted() =
        match rnd.Next(2) with
        | 0 -> Units.BomberTransport.Bombs 2
        | _ -> Units.BomberTransport.Infantry(Units.Health 1.0f)

    let rndCarrier docked coords : Units.UnitInfo =
        let carried_units =
            [|
                while rnd.Next(4) <= 2 do
                    yield match rnd.Next(2) with
                          | 0 -> Units.CarriedAircraft.Fighter(Units.Health 1.0f)
                          | _ ->
                            Units.CarriedAircraft.Bomber(
                                rndBomberLifted(),
                                Units.Health(1.0f))
            |]
        { coords = coords ;
          moves = Units.carrier_range ;
          health = 1.0f ;
          specific = Units.Carrier(docked, carried_units) }

    let player_units : Units.UnitInfo[][]=
        [|
            for i in 0..3 do
                yield
                    [|
                        for j in 0..99 do
                            let x = rnd.Next(width)
                            let y = rnd.Next(width)

                            let coords = ofSq(x, y)
                            yield match (Terrain.getHex terr coords) with
                                  | Terrain.Land ->
                                    match (rnd.Next(11)) with
                                    | 0 ->
                                        { coords = coords ;
                                          moves = Units.anti_aircraft_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.AntiAircraft }
                                    | 1 ->
                                        { coords = coords ;
                                          moves = Units.artillery_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Artillery }
                                    | 2 ->
                                        { coords = coords ;
                                          moves = Units.battleship_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Battleship(Units.Docked) }
                                    | 3 ->
                                        { coords = coords ;
                                          moves = Units.infantry_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Infantry }
                                    | 4 ->
                                        { coords = coords ;
                                          moves = Units.tank_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Tank }
                                    | 5 ->
                                        { coords = coords ;
                                          moves = Units.destroyer_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Destroyer(Units.Docked) }
                                    | 6 -> rndTransport Units.Docked coords
                                    | 7 -> rndCarrier Units.Docked coords
                                    | 8 ->
                                        { coords = coords ;
                                          moves = Units.submarine_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Submarine(Units.Docked, Units.NotStealthy) }
                                    | 9 ->
                                        { coords = coords ;
                                          moves = Units.fighter_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Fighter(Units.Landed, Units.Fuel Units.fighter_fuel_range) }
                                    | 10 ->
                                        { coords = coords ;
                                          moves = Units.bomber_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Bomber(Units.Landed, Units.Fuel Units.bomber_fuel_range, rndBomberLifted()) }
                                    | _ ->
                                        { coords = coords ;
                                          moves = Units.destroyer_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Destroyer(Units.Docked) }
                                  | Terrain.Sea ->
                                    match (rnd.Next(7)) with
                                    | 0 ->
                                        { coords = coords ;
                                          moves = Units.battleship_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Battleship(Units.NotDocked) }
                                    | 1 -> rndTransport Units.NotDocked coords
                                    | 2 -> rndCarrier Units.NotDocked coords
                                    | 3 ->
                                        { coords = coords ;
                                          moves = Units.submarine_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Submarine(Units.NotDocked, Units.NotStealthy) }
                                    | 4 ->
                                        { coords = coords ;
                                          moves = Units.fighter_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Fighter(Units.Airborne, Units.Fuel Units.fighter_fuel_range) }
                                    | 5 ->
                                        { coords = coords ;
                                          moves = Units.bomber_range ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Bomber(Units.Airborne, Units.Fuel Units.bomber_fuel_range, rndBomberLifted()) }
                                    | _ -> 
                                    { coords = coords ;
                                      moves = Units.destroyer_range ;
                                      health = 1.0f ;
                                      specific = Units.UnitTypes.Destroyer(Units.NotDocked) }
                    |]
        |]

    let gs = { default_gs with player_units = player_units }

    let doRound (gs : GameState.GameState) =
        let possible_orders =
            [| for player in 0 .. gs.player_units.Length - 1 do
                yield async { return Ai.getValidOrders gs player }
            |]
            |> Async.Parallel
            |> Async.RunSynchronously

        let random_orders =
            possible_orders
            |> Array.map (fun units ->
                units
                |> Array.map (fun orders ->
                    match orders.Length with
                    | 0 -> Orders.DoNothing
                    | n ->  orders.[rnd.Next(n - 1)]))

        GameStateUpdate.update gs random_orders

    let mutable gs' = gs
    for i in 1 .. 10 do
        printfn "Starting round %d" i
        gs' <- doRound gs'

    ()