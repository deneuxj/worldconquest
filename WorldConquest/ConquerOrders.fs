module ConquerOrders

open GameState
open Orders

let extractConquests (player : int) (orders : Order[]) =
    orders
    |> Array.choose(function
        | Orders.Conquer (rsc, target, _) -> Some (rsc, target)
        | _ -> None)

