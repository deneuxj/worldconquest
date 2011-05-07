// Learn more about F# at http://fsharp.net

namespace WorldConquest.PexTests

open Microsoft.Pex.Framework

open GameState
open GameStateUpdate
open Ai
open HexTiling
open Units

[<PexClass>]
type GameStateUpdateTests() =
    let width = 32

    let terr =
        let inMid x = x >= width / 4 && x < 3 * width / 4
        Array2D.init width width (fun i j -> if inMid i && inMid j then Terrain.Land else Terrain.Sea)

    [<PexMethod>]
    member x.Test1(resources, player_units, selected_orders_idxs : int [][]) =
        PexAssume.IsNotNull(resources)
        PexAssume.AreElementsNotNull(resources)
        PexAssume.IsNotNull(player_units)
        PexAssume.AreElementsNotNull(player_units)
        for u : Units.UnitInfo in Seq.concat player_units do
            PexAssume.IsNotNull(u)
            let (SquareCoords(x, y)) = fromHex u.coords
            PexAssume.InRange(x, 0, width - 1)
            PexAssume.InRange(y, 0, width - 1)

        PexAssume.IsNotNull(selected_orders_idxs)
        PexAssume.AreElementsNotNull(selected_orders_idxs)
        PexAssume.IsTrue(Array.length player_units = Array.length selected_orders_idxs)
        let gs = GameState.GameState.Create(terr, resources, player_units)

        let player_orders =
            [|
                for i in 0 .. Array.length player_units - 1 do
                    PexAssume.IsNotNull(player_units.[i])
                    PexAssume.AreElementsNotNull(player_units.[i])

                    let valid_orders = getValidOrders gs i
                    PexAssume.IsTrue(valid_orders.Length = Array.length (selected_orders_idxs.[i]))

                    let orders =
                        Array.zip valid_orders selected_orders_idxs.[i]
                        |> Array.map (fun (orders, idx) ->
                            PexAssume.InRange(idx, 0, Array.length orders - 1)
                            orders.[idx])

                    yield orders
            |]

        GameStateUpdate.update gs player_orders |> ignore

    // Test1 conclusions:
    // Needs lot of PexAssume.IsNotNull, despite the fact that most of the F# types don't accept null as a value.
    // Too hard to find something interesting.
    // Managed to expose IndexOutOfRange exceptions if units are created with outside-of-map coordinates.

    // Fuel in aircrafts that move decreases
    [<PexMethod(MaxConditions=2000, MaxConstraintSolverTime=8, MaxBranches=20000)>]
    member x.TestFuelDecreases(order_oracle : int[][], is_bomber : bool, bomber_transport) =
        let max_rounds = 2
        let health = 1.0f
        let coords = SquareCoords(8, 8) |> toHex

        PexAssume.IsNotNull(order_oracle)
        PexAssume.AreElementsNotNull(order_oracle)

        PexAssume.IsTrue(order_oracle.Length >= max_rounds)
        PexAssume.Implies(is_bomber, fun () -> PexAssume.IsNotNull(bomber_transport))
        PexAssume.IsNotNull(coords)

        let specific =
            if is_bomber then
                Bomber(Landed.Landed, Fuel bomber_fuel_range, bomber_transport)
            else
                Fighter(Landed.Landed, Fuel fighter_fuel_range)
        
        let unit =
            { coords = coords ;
              health = health ;
              specific = specific }

        let gs = GameState.Create(terr, [], [| [|unit|] |])

        let rec simulate gs round =
            let valid_orders = getValidOrders gs 0
            let orders =
                Array.zip valid_orders order_oracle.[round]
                |> Array.map (fun (orders, i) ->
                    //PexAssume.InRange(i, 0, Array.length orders - 1)
                    orders.[i])
            
            let gs', _ = update gs [| orders |]
            PexAssert.Implies(
                match orders.[0] with
                | Orders.Bomb _
                | Orders.Move _ -> true
                | _ -> false
                ,
                fun () ->
                    PexAssert.IsTrue(
                        match gs.player_units.[0].[0].specific, gs'.player_units.[0].[0].specific with
                        | Bomber(_, Fuel f, _), Bomber(_, Fuel f', _) -> f' < f
                        | Fighter(_, Fuel f), Fighter(_, Fuel f') -> f' < f
                        | _ -> false
                    )
            )

            if (round + 1) < max_rounds then
                simulate gs' (round + 1)
            else
                ()

        simulate gs 0

    // Conclusions: Hit path limitations, did not generate any test.

    [<PexMethod>]
    member x.TestGrowUnitTree(embark, disembark, units) =
        PexAssume.IsNotNull embark
        PexAssume.AreElementsNotNull embark
        PexAssume.IsNotNull disembark
        PexAssume.AreElementsNotNull disembark
        PexAssume.IsNotNull units
        PexAssume.AreElementsNotNull units

        growUnitTree embark disembark units