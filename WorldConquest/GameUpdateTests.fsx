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

    |> assertTrue (printfn "%s") "BOMBARD"