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
