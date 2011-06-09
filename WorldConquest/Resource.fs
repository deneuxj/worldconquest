module Resource

type Production =
    | Infantry
    | Tank
    | Artillery
    | AntiAircraft
    | Transport
    | Destroyer
    | Submarine
    | Battleship
    | Carrier
    | Fighter
    | Bomber

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
