#r @"bin\Debug\XNAUtils.dll"

#load "PathFinding.fs"
#load "HexTiling.fs"
#load "Terrain.fs"
#load "Units.fs"
#load "Resource.fs"
#load "GameState.fs"
#load "Orders.fs"
#load "DockOrders.fs"
#load "AttackOrders.fs"
#load "MoveOrders.fs"
#load "EmbarkOrders.fs"
#load "ConquerOrders.fs"
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
    let gs', _ = GameStateUpdate.update gs [||]

    assertTrue (printfn "%s") "ZERO" (gs = gs')

let moveTest() =
    let start = ofSq(width / 2, width / 2)
    let goal = ofSq(width / 2 - 1, width / 2)

    let units_and_orders : (Units.UnitInfo * _)[] =
        [|
            { coords = start ;
                health = 1.0f ;
                specific = Units.UnitTypes.Tank },
            Orders.Move { path = (findPath start goal).Value; unit = GameState.Root 0 }
        |]

    let gs =
        { default_gs with
            player_units = [| Array.unzip units_and_orders |> fst |]
        }

    let gs', _ = GameStateUpdate.update gs [| Array.unzip units_and_orders |> snd |]

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
                specific = Units.UnitTypes.Artillery },
            Orders.Bombard u1
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
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

    let gs', _ =
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
                specific = Units.UnitTypes.Tank },
            Orders.DirectAttack(u1, [])
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
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

    let gs', _ =
        GameStateUpdate.update gs
            [|
                Array.unzip p0 |> snd ;
                Array.unzip p1 |> snd
            |]

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
                specific = Units.UnitTypes.Tank },
            Orders.DirectAttack(u1, [])
        |]

    let p1 : (Units.UnitInfo * _)[] =
        [|
            { coords = u1 ;
                health = 1.0f ;
                specific = Units.UnitTypes.Infantry },
            Orders.DoNothing ;

            { coords = u1 ;
                health = 1.0f ;
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

    let gs', _ =
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
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.AntiAircraft }
                                    | 1 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Artillery }
                                    | 2 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Battleship(Units.Docked) }
                                    | 3 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Infantry }
                                    | 4 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Tank }
                                    | 5 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Destroyer(Units.Docked) }
                                    | 6 -> rndTransport Units.Docked coords
                                    | 7 -> rndCarrier Units.Docked coords
                                    | 8 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Submarine(Units.Docked, Units.NotStealthy) }
                                    | 9 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Fighter(Units.Landed, Units.Fuel Units.fighter_fuel_range) }
                                    | 10 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Bomber(Units.Landed, Units.Fuel Units.bomber_fuel_range, rndBomberLifted()) }
                                    | _ ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Destroyer(Units.Docked) }
                                  | Terrain.Sea ->
                                    match (rnd.Next(7)) with
                                    | 0 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Battleship(Units.NotDocked) }
                                    | 1 -> rndTransport Units.NotDocked coords
                                    | 2 -> rndCarrier Units.NotDocked coords
                                    | 3 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Submarine(Units.NotDocked, Units.NotStealthy) }
                                    | 4 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Fighter(Units.Airborne, Units.Fuel Units.fighter_fuel_range) }
                                    | 5 ->
                                        { coords = coords ;
                                          health = 1.0f ;
                                          specific = Units.UnitTypes.Bomber(Units.Airborne, Units.Fuel Units.bomber_fuel_range, rndBomberLifted()) }
                                    | _ -> 
                                    { coords = coords ;
                                      health = 1.0f ;
                                      specific = Units.UnitTypes.Destroyer(Units.NotDocked) }
                    |]
        |]

    let gs = { default_gs with player_units = player_units }

    let doRound (gs : GameState.GameState) =
        let possible_orders =
            [| for player in 0 .. gs.player_units.Length - 1 do
                yield Ai.getValidOrders gs player
            |]

        let random_orders =
            possible_orders
            |> Array.map (fun units ->
                units
                |> Array.map (fun orders ->
                    match orders.Length with
                    | 0 -> Orders.DoNothing
                    | n ->  orders.[rnd.Next(n - 1)]))

        let gs', getRootFromOldIdx = GameStateUpdate.update gs random_orders

        let check() =
            Seq.zip (Seq.initInfinite id) random_orders
            |> Seq.fold (fun is_ok (i, player_orders) ->
                is_ok                   
               ) true                

        gs'

    let mutable gs' = gs
    for i in 1 .. 10 do
        printfn "Starting round %d" i
        gs' <- doRound gs'

    ()

let testCapture() =
    let u0 = ofSq(width / 2, width / 2)
    let u1 = ofSq(width / 2 + 1, width / 2 + 1)
    let rsc = Resource.Factory (Some { prod = Resource.Production.AntiAircraft; turns_left = 1 })

    let player_units : (Units.UnitInfo)[][] =
        [|
            [|
                { coords = u0 ;
                    health = 1.0f ;
                    specific = Units.UnitTypes.Tank }                
            |]
            [||]
        |]

    let order =
        [|
            [|
                Orders.Conquer(rsc, u1, [])
            |]
            [||]
        |]

    let gs =
        GameState.GameState.Create(
            terr,
            [(u1, rsc, Some (GameState.PlayerId 1))],
            player_units)

    let gs', _ = GameStateUpdate.update gs order

    let good_length_p0 =
        1 = Seq.length gs'.resources_of.[0]

    let good_length_p1 =
        0 = Seq.length gs'.resources_of.[1]
    
    let prod_reset =
        gs'.resources_of.[0]
        |> Seq.forall (fun (_, rsc) ->
            match rsc with
            | Resource.Factory None -> true
            | _ -> false)

    good_length_p0 |> assertTrue (printfn "%s") "Resource captured"
    good_length_p1 |> assertTrue (printfn "%s") "Resource lost"
    prod_reset |> assertTrue (printfn "%s") "Production of captured factory reset"

let testProduction() =
    let u0 = ofSq(width / 2, width / 2)
    let fac0 = Resource.Factory (Some { prod = Resource.Production.AntiAircraft; turns_left = 1 })
    let oil = Resource.Oil
    let iron = Resource.Iron
    let wood = Resource.Wood

    let gs =
        let no_units = [|[||];[||]|]
        GameState.GameState.Create(
            terr,
            [(u0, fac0, Some (GameState.PlayerId 0))
             (u0, fac0, Some (GameState.PlayerId 1))
             (u0, oil, Some (GameState.PlayerId 1))
             (u0, iron, Some (GameState.PlayerId 1))
             (u0, wood, Some (GameState.PlayerId 1))],
            no_units)

    let gs', _ =
        let no_orders = [|[||];[||]|]
        GameStateUpdate.update gs no_orders 

    let good_units_length_p0 =
        0 = gs'.player_units.[0].Length

    let prod_halted_p0 =
        gs'.resources_of.[0]
        |> Seq.forall (fun (_, rsc) -> fac0 = rsc)

    let good_units_length_p1 =
        1 = gs'.player_units.[1].Length

    let prod_reset_p1 =
        gs'.resources_of.[1]
        |> Seq.forall (fun (_, rsc) ->
            match rsc with
            | Resource.Factory None -> true
            | Resource.Oil | Resource.Wood | Resource.Iron -> true
            | Resource.Factory (Some _ ) -> false
            | _ -> false)

    good_units_length_p0 |> assertTrue (printfn "%s") "Not enough resources, no unit produced"
    prod_halted_p0 |> assertTrue (printfn "%s") "Not enough resources, production halted"
    good_units_length_p1 |> assertTrue (printfn "%s") "New unit produced"
    prod_reset_p1 |> assertTrue (printfn "%s") "Production completed and reset"
