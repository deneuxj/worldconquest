module GameState

open Units
open HexTiling
open Resource
open Terrain

type PlayerId = PlayerId of int

type GameState =
    {  terrain : Terrain[,]
       getResourceAt : HexCoords -> (Resource * PlayerId option) option
       getResourcesOf : PlayerId -> HexCoords list
       player_units : UnitInfo[][]  }

type Order =
    | Move of HexCoords list
    | Bombard of HexCoords
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
                | Transport(_, _ :: _) | Bomber(_, _, BomberTransport.Infantry _) -> true
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


let moveUnits (units : UnitInfo[]) (destinations : HexCoords option[]) =
    let moveUnit (u : UnitInfo) (dest : HexCoords option) =
        match dest with
        | Some dest ->
            { u with coords = dest }
        | None -> u
    Array.map2 moveUnit units destinations

let validateMoves terrain (getResourceAt : HexCoords -> Resource option) (units : UnitInfo[]) (destinations : HexCoords option[]) =
    let isValid (u : UnitInfo) (dest : HexCoords option) =
        false
    Array.map2 isValid units destinations