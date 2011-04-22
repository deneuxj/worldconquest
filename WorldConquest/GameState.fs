module GameState

open Units
open HexTiling
open Resource
open Terrain

type PlayerId = PlayerId of int

type GameState =
    {  terrain : Terrain[,]
       getResourceAt : HexCoords -> (Resource * PlayerId option) option
       getResourcesOf : PlayerId -> (HexCoords * Resource) list
       player_units : UnitInfo[][]  }

type UnitIndex =
    | Root of int
    | Transported of int * int // A unit inside another unit
    | Transported2 of int * int * int // A unit inside a unit which is itself inside another unit.

type Order =
    | Move of HexCoords list
    | Bombard of HexCoords
    | Bomb of HexCoords * HexCoords list // Coords of target, path to bomb-dropping site.
    | Unload of HexCoords list * HexCoords
    | Load of HexCoords list * int // Id of the unit
    | DirectAttack of HexCoords list
    | Conquer of HexCoords list
    | DockAt of HexCoords list
    | LandAt of HexCoords list
    | DoNothing

let getOrder (gs : GameState) (player : int) =
    let enemy_positions =
        seq {
            for i in 0..gs.player_units.Length-1 do
                if i <> player then
                    yield! gs.player_units.[i] |> Array.map (fun u -> u.coords)
        }
        |> Set.ofSeq

    let width = gs.terrain.GetLength(0)
    let dist c0 c1 =
        distWrapSq width (fromHex c0) (fromHex c1)
        |> float32

    fun (unit : UnitInfo) (destination : HexCoords) ->
        let dest_terrain = getHex gs.terrain destination

        let canMove dist max_moves =
            match unit.specific with
            // Land unit and sea unit can move through a certain terrain type
            // Units cannot move to a tile occupied by an enemy unit.
            | LandUnit | SeaUnit | Docked ->
                let terr_type =
                    match unit.specific with
                    | LandUnit -> Land
                    | SeaUnit | Docked -> Sea
                    | _ -> failwith "Unreachable"

                PathFinding.find
                    dist
                    (fun c -> neighboursOfWrapSq width (fromHex c)
                              |> List.filter (fun (SquareCoords(_, y) as c) -> y >= 0 && y < width && (getSq gs.terrain c) = terr_type)
                              |> List.map toHex
                              |> List.filter (fun c -> not <| enemy_positions.Contains(c))
                              )
                    (fun _ -> 1.0f)
                    (unit.coords)
                    (float32 max_moves)
            
            // Aircrafts can fly anywhere, but not through tiles occupied by enemies.
            | Fighter(_, Fuel fuel)
            | Bomber(_, Fuel fuel, _) ->
                let moves =
                    min max_moves fuel
                PathFinding.find
                    dist
                    (fun c -> neighboursOfWrapSq width (fromHex c)
                              |> List.map toHex
                              |> List.filter (fun c -> not <| enemy_positions.Contains(c)))
                    (fun _ -> 1.0f)
                    (unit.coords)
                    (float32 moves)

            // Should not be reachable.
            | Landed
            | AirUnit -> None

        let canMoveToNeighbour destination =
            let ngbh = neighboursOfWrapSq width (fromHex destination)
            let distNgbh c =
                ngbh
                |> List.map(fun n -> dist (toHex n) c)
                |> List.min
            canMove distNgbh (unit.moves - 1)

        let canBombard() =
            enemy_positions.Contains(destination)
            && dist unit.coords destination <= float32 (getBombardRange unit.specific)
            && seq {
                for i in 0..gs.player_units.Length-1 do
                    if i <> player then
                        yield! gs.player_units.[i] |> Array.filter (fun u -> u.coords = destination)
                }
               |> Seq.exists (fun target -> canBombard(unit, target))

        let canBomb() =
            if
                enemy_positions.Contains(destination)
                && seq {
                        for i in 0..gs.player_units.Length-1 do
                            if i <> player then
                                yield! gs.player_units.[i] |> Array.filter (fun u -> u.coords = destination)
                    }
                   |> Seq.exists (fun target -> canBomb(unit, target))
            then
                canMoveToNeighbour destination
            else
                None


        let canDirectAttack() =
            if
                enemy_positions.Contains(destination)
                && seq {
                        for i in 0..gs.player_units.Length-1 do
                            if i <> player then
                                yield! gs.player_units.[i] |> Array.filter (fun u -> u.coords = destination)
                    }
                   |> Seq.exists (fun target -> canDirectAttack(unit, target))
            then
                canMoveToNeighbour destination
            else
                None

        let canUnload() =
            if
                // The unit is a non-empty transport or bomber
                match unit.specific with
                | Transport(_, [||]) -> false
                | Transport(_, _) | Bomber(_, _, BomberTransport.Infantry _) -> true
                | _ -> false
                &&
                // The unload location is on land
                getHex gs.terrain destination = Land
            then
                // Is it possible to move to a neighbour of the unload location?
                match canMoveToNeighbour destination with
                | Some path -> Some (path, destination)
                | None -> None
            else
                None

        let canLoad() =
            let units_at_destination =
                seq { 0..gs.player_units.[player].Length-1 }
                |> Seq.filter (fun i -> gs.player_units.[player].[i].coords = destination)
            
            let candidate_receivers =
                units_at_destination
                |> Seq.filter
                    (fun i ->
                        gs.player_units.[player].[i].specific
                        |>
                        match unit.specific with
                        | Infantry ->
                            function
                            | Transport _
                            | Bomber(Landed.Landed, _, BomberTransport.Bombs _) -> true
                            | _ -> false
                        | LandUnit ->
                            function
                            | Transport _ -> true
                            | _ -> false
                        | Fighter _
                        | Bomber _ ->
                            function
                            | Carrier _ -> true
                            | _ -> false
                        | _ -> fun _ -> false)
                |> List.ofSeq

            match candidate_receivers with
            | [] -> printfn "No candidate receiver"; None
            | idx :: _ ->
                match canMoveToNeighbour destination with
                | Some path -> Some (path, idx)
                | None -> None

        let canConquer() =
            if
                match unit.specific with
                | LandUnit -> true
                | _ -> false
                &&
                match gs.getResourceAt destination with
                | Some(_, Some (PlayerId i)) -> i <> player
                | Some(_, None) -> true
                | None -> false
            then
                canMove (dist destination) unit.moves
            else
                None

        let canDockAt() =
            if
                match unit.specific with
                | SeaUnit -> true
                | _ -> false
                &&
                match gs.getResourceAt destination with
                | Some(Resource.Harbour, Some (PlayerId i))
                | Some(Resource.Factory, Some (PlayerId i)) -> i = player
                | Some _
                | None -> false
            then
                canMove (dist destination) unit.moves
            else
                None

        let canLandAt() =
            if
                match unit.specific with
                | Landed
                | AirUnit -> true
                | _ -> false
                &&
                match gs.getResourceAt destination with
                | Some(Resource.Airfield, Some(PlayerId i)) -> i = player
                | Some _
                | None -> false
            then
                canMove (dist destination) unit.moves
            else
                None

        [
            match canConquer() with
            | Some path -> yield Conquer path
            | None -> ()

            if canBombard() then
                yield Bombard destination

            match canBomb() with
            | Some path -> yield Bomb (destination, path)
            | None -> ()

            match canDirectAttack() with
            | Some path -> yield DirectAttack path
            | None -> ()

            match canLoad() with
            | Some x -> yield Load x
            | None -> ()

            match canUnload() with
            | Some x -> yield Unload x
            | None -> ()

            match canLandAt() with
            | Some x -> yield LandAt x
            | None -> ()

            match canDockAt() with
            | Some x -> yield DockAt x
            | None -> ()

            match canMove (dist destination) unit.moves with
            | Some path -> yield Move path
            | None -> ()
        ]

// f maps a unit to a sequence of some generic data
// g filters the generic data for units that are inside a transport.
// g is needed to e.g. prevent artilleries to fire from transports.
let playerUnitMap (f : UnitIndex * UnitInfo -> 'T[]) g (units : UnitInfo[]) =
    [|
        for i in 0..units.Length-1 do
            let u = units.[i]
            yield f (Root i, u)

            match u.specific with
            | Transport(_, transported) ->
                for i2 in 0..transported.Length-1 do
                    let t = transported.[i2]
                    let unit_type =
                        match t with
                        | TransportedUnit.Infantry _ -> Infantry
                        | TransportedUnit.Tank _ -> Tank
                        | TransportedUnit.Artillery _ -> Artillery
                        | TransportedUnit.AntiAircraft _ -> AntiAircraft
                    let u' =
                        {  coords = u.coords;
                           health = t.Health;
                           moves = getMovementRange unit_type;
                           specific = unit_type  }
                    yield f (Transported(i, i2), u') |> Array.filter g
            | Bomber(_, _, BomberTransport.Infantry(Health h)) ->
                let u' =
                    {  coords = u.coords;
                       health = h;
                       moves = getMovementRange Infantry
                       specific = Infantry  }
                yield f (Transported(i, 0), u')
            | Carrier(_, aircrafts) ->
                for i2 in 0..aircrafts.Length-1 do
                    let plane = aircrafts.[i2]
                    let unit_type =
                        match plane with
                        | CarriedAircraft.Fighter _ -> Fighter(Landed, Fuel fighter_fuel_range)
                        | CarriedAircraft.Bomber (transported, _) -> Bomber(Landed, Fuel bomber_fuel_range, transported)
                    let u' =
                        {  coords = u.coords;
                           health = plane.Health;
                           moves = getMovementRange unit_type;
                           specific = unit_type  }
                    yield f (Transported(i, i2), u')

                    match unit_type with
                    | Bomber(_, _, BomberTransport.Infantry(Health h)) ->
                        let u'' =
                            {  coords = u.coords;
                               health = h;
                               moves = getMovementRange Infantry
                               specific = Infantry  }
                        yield f (Transported2(i, i2, 0), u'')
                    | _ -> ()
            | _ -> ()
    |]

type UnitTempState =
    | Embarking of UnitInfo * int
    | Killed of UnitInfo
    | Normal of UnitInfo

let mkFetchOrderMap f orders =
    let it = (orders :> System.Collections.Generic.IEnumerable<Order>).GetEnumerator()
    fun u ->
        let order = it.MoveNext() |> fun _ -> it.Current
        f (u, order)

let executeOrders (gs : GameState) (player : int) (orders : Order[]) =
    let width = gs.terrain.GetLength(0)
    let executeOrder (u : UnitInfo, order : Order) =
        let getDestination = function
        | [] -> u.coords
        | path -> path |> List.rev |> List.head

        match order with
        | Move path | DirectAttack path | Conquer path | DockAt path | LandAt path ->
            Normal { u with coords = getDestination path }
        | Bomb (_, path) ->
            match u.specific with
            | Bomber(Airborne, fuel, BomberTransport.Bombs n) ->
                Normal { u with coords = getDestination path;
                                specific = Bomber(Airborne, fuel, BomberTransport.Bombs (n - 1)) }
            | _ -> failwith "Only airborne bombers carrying bombs may bomb"
        | Bombard _ ->
            Normal u
        | DoNothing ->
            Normal u
        | Load (path, idx_transport) ->
            Embarking ({ u with coords = getDestination path }, idx_transport)
        | Unload (path, _) ->
            match u.specific with
            | Transport(docked, _) ->
                Normal { u with coords = getDestination path;
                                specific = Transport(docked, [||]) }
            | Bomber(landed, fuel, _) ->
                Normal { u with coords = getDestination path;
                                specific = Bomber(landed, fuel, BomberTransport.Bombs 0) }
            | _ -> failwith "Only transports and bombers can unload their transported units"

    let fetchAndExecuteOrder =
        mkFetchOrderMap
            (fun ((_,u), order) -> [| executeOrder (u, order) |])
            orders
        
    playerUnitMap fetchAndExecuteOrder (fun _ -> true) gs.player_units.[player]
    |> Array.concat


type AttackType =
    | Remote
    | Melee

type AttackOrder =
    {  player : PlayerId
       unit : int
       coords : HexCoords
       attack : AttackType  }

let extractAttackOrders (units : UnitInfo[]) (player : int) (orders : Order[]) =
    let getRootUnitOrder ((idx : UnitIndex, u : UnitInfo), order : Order) =
        match idx with
        | Root idx ->
            let coords_and_attack =
                match order with
                | Order.Bombard coords -> Some (coords, Remote)
                | Order.DirectAttack path -> Some (path |> List.rev |> List.head, Melee)
                | Order.Bomb (coords, _) -> Some (coords, Remote)
                | _ -> None
            match coords_and_attack with
            | Some (coords, attack) ->
                [|
                    {  player = PlayerId player;
                       unit = idx;
                       coords = coords;
                       attack = attack  }
                |]
            | None -> Array.empty
        | _ -> Array.empty

    let fetchRootUnitOrder = mkFetchOrderMap getRootUnitOrder orders
    playerUnitMap fetchRootUnitOrder (fun _ -> true) units
    |> Array.concat

let injureUnits (gs : GameState) (player : int) (orders : AttackOrder[]) =

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

    [
        for attack in orders do
            match enemyUnits.TryGetValue(attack.coords) with
            | true, units ->                
                let attacker = gs.player_units.[player].[attack.unit]
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

                    let damage = 3.0f + bonus
                    let health_points =
                        match victim_unit.specific with
                        | Infantry -> infantry_hp
                        | Tank -> tank_hp
                        | Transport _ -> transport_hp
                        | Destroyer _ -> destroyer_hp
                        | Submarine _ -> submarine_hp
                        | Carrier _ -> carrier_hp
                        | Fighter _ -> fighter_hp
                        | Bomber _ -> bomber_hp
                        | Artillery -> artillery_hp
                        | AntiAircraft -> anti_aircraft_hp
                        | Battleship _ -> battleship_hp
                    yield (vp_id, victim_idx, damage / health_points)

            | false, _ -> ()
    ]

