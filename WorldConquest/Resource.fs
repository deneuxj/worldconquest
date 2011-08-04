module Resource

type Production =
    | Infantry = 0
    | Tank = 1
    | Artillery = 2
    | AntiAircraft = 3
    | Transport = 4
    | Destroyer = 5
    | Submarine = 6
    | Battleship = 7
    | Carrier = 8
    | Fighter = 9
    | Bomber = 10

type ProductionState =
    { prod : Production
      turns_left : int }

type Resource =
    | Oil
    | Wood
    | Iron
    | Factory of ProductionState option
    | Harbour
    | Airfield

let countMinRsc rscs =
    let oil, wood, iron =
        rscs
        |> List.fold(fun (oil, wood, iron) rsc ->
            match rsc with
            | Oil -> (oil + 1, wood, iron)
            | Wood -> (oil, wood + 1, iron)
            | Iron -> (oil, wood, iron + 1)
            | Factory _ | Harbour | Airfield -> (oil, wood, iron))
            (0, 0, 0)

    oil
    |> min wood
    |> min iron

let updateProduction num_min_rsc rsc pos =
    match rsc with
    | Factory(Some state) ->
        if num_min_rsc > 0 then
            if state.turns_left = 1 then
                let unit_specific =
                    match state.prod with
                    | Production.Infantry -> Units.UnitTypes.Infantry
                    | Production.Tank -> Units.UnitTypes.Tank
                    | Production.Artillery -> Units.UnitTypes.Artillery
                    | Production.AntiAircraft -> Units.UnitTypes.AntiAircraft
                    | Production.Transport -> Units.UnitTypes.Transport(Units.Docked, [||])
                    | Production.Destroyer -> Units.UnitTypes.Destroyer(Units.Docked)
                    | Production.Submarine -> Units.UnitTypes.Submarine(Units.Docked, Units.NotStealthy)
                    | Production.Battleship -> Units.UnitTypes.Battleship(Units.Docked)
                    | Production.Carrier -> Units.UnitTypes.Carrier(Units.Docked, [||])
                    | Production.Fighter -> Units.UnitTypes.Fighter(Units.Landed, Units.Fuel Units.fighter_fuel_range)
                    | Production.Bomber -> Units.UnitTypes.Bomber(Units.Landed, Units.Fuel Units.bomber_fuel_range, Units.BomberTransport.Bombs Units.bomber_max_bombs)
                    | _ -> failwith "Invalid enum value"
                let unit_info : Units.UnitInfo =
                    { coords = pos; health = 1.0f; specific = unit_specific }
                Factory None, Some unit_info, num_min_rsc - 1
            else
                Factory (Some { state with turns_left = state.turns_left - 1 }), None, num_min_rsc - 1
        else
            rsc, None, num_min_rsc
    
    | Factory None
    | Oil
    | Wood
    | Iron
    | Harbour
    | Airfield -> rsc, None, num_min_rsc
