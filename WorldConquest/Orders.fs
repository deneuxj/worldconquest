module Orders

open GameState
open HexTiling
open Units
open Terrain

type MoveInfo =
    { path : HexCoords list
      unit : UnitIndex }

type LoadInfo =
    { path : HexCoords list
      transport : int
      unit : UnitIndex }

type Order =
    | Move of MoveInfo
    | Bombard of HexCoords
    | Bomb of HexCoords * HexCoords list // Coords of target, path to bomb-dropping site.
    | Unload of HexCoords list * HexCoords // Applies to a transport, carrier or bomber, not the transported unit.
    | Load of LoadInfo
    | DirectAttack of HexCoords * HexCoords list // Coords of unit to attack, path to the site.
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

    let units = gs.player_units.[player]

    fun (idx : UnitIndex) (destination : HexCoords) ->
        let unit = getUnitByIndex units idx
        let dest_terrain = getHex gs.terrain destination

        // Filter invalid orders depending on whether the units is on board another unit.

        let filterValidOrders =
            let transport_filter order =
                match order with
                | Load _
                | Bombard _
                | Conquer _
                | DirectAttack _ -> false

                | DoNothing
                | Move _
                | Unload _ -> true

                | Bomb _
                | DockAt _
                | LandAt _ -> failwith <| sprintf "Invalid order %A for a land unit" order

            let carrier_filter order =
                match order with
                | Unload _ // Bombers are allowed to unload their paratroopers while inside a carrier
                | Move _
                | Load _
                | LandAt _
                | DoNothing
                | DirectAttack _
                | Bomb _ -> true

                | Bombard _
                | Conquer _
                | DockAt _ -> failwith <| sprintf "Invalid order %A for an aircraft" order
                
            let bomber_filter order =
                match order with
                | Move _
                | DoNothing -> true

                | DirectAttack _
                | Conquer _
                | Load _ -> false

                | Unload _ -> failwith "Invalid unload order for an infantry"
                | LandAt _ -> failwith "Invalid land order for an infantry"
                | DockAt _ -> failwith "Invalid dock order for an infantry"
                | Bombard _ -> failwith "Invalid bombard order for an infantry"
                | Bomb _ -> failwith "Invalid bomb order for an infantry"
                
            let acceptAll _ = true

            match idx with
            | Root _ -> acceptAll
            | Transported(root, _) ->
                match units.[root].specific with
                | Units.Transport _ -> transport_filter
                | Units.Carrier _ -> carrier_filter
                | Units.UnitTypes.Bomber _ -> bomber_filter
                | _ -> acceptAll
            | Transported2 _ -> bomber_filter

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
            canMove distNgbh (getMovementRange unit.specific - 1)

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
            | [] -> []
            | _ :: _ ->
                match canMoveToNeighbour destination with
                | Some path ->
                    candidate_receivers
                    |> List.map (fun i -> Load { path = path; transport = i; unit = idx })
                | None -> []

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
                canMove (dist destination) (getMovementRange unit.specific)
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
                canMove (dist destination) (getMovementRange unit.specific)
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
                canMove (dist destination) (getMovementRange unit.specific)
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
            | Some path -> yield DirectAttack (destination, path)
            | None -> ()

            yield! canLoad()

            match canUnload() with
            | Some x -> yield Unload x
            | None -> ()

            match canLandAt() with
            | Some x -> yield LandAt x
            | None -> ()

            match canDockAt() with
            | Some x -> yield DockAt x
            | None -> ()

            match canMove (dist destination) (getMovementRange unit.specific) with
            | Some path -> yield Move { path = path; unit = idx }
            | None -> ()
        ]
        |> List.filter filterValidOrders


let playerUnitZipIter (f : UnitIndex * UnitInfo * 'T -> unit) (units : UnitInfo[]) getNextX =
    for i in 0..units.Length-1 do
        let u = units.[i]
        f(Root i, u, getNextX())

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
                        specific = unit_type  }
                f(Transported(i, i2), u', getNextX())
        | Bomber(_, _, BomberTransport.Infantry(Health h)) ->
            let u' =
                {  coords = u.coords;
                    health = h;
                    specific = Infantry  }
            f(Transported(i, 0), u', getNextX())
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
                        specific = unit_type  }
                f(Transported(i, i2), u', getNextX())

                match unit_type with
                | Bomber(_, _, BomberTransport.Infantry(Health h)) ->
                    let u'' =
                        {  coords = u.coords;
                            health = h;
                            specific = Infantry  }
                    f(Transported2(i, i2, 0), u'', getNextX())
                | _ -> ()
        | _ -> ()
    

// Turn the unit tree into a flat array, and map each unit to an array.
// This is used e.g. to generate all possible moves for each unit.
// f maps a unit to an array of some generic data
let playerUnitMap (f : UnitIndex * UnitInfo -> 'T[]) (units : UnitInfo[]) =
    let res = new ResizeArray<'T[]>()
    playerUnitZipIter (fun (idx, unit, _) -> res.Add(f(idx, unit))) units id
    res.ToArray()

let playerUnitZipMap f units (orders : 'O[]) =
    let getNextOrder =
        let i = ref 0
        fun () ->
            let r = orders.[!i]
            i := !i + 1
            r

    let res = new ResizeArray<'T[]>()
    playerUnitZipIter (fun x -> res.Add(f x)) units getNextOrder
    res.ToArray()
