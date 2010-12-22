module HexTiling

type HexCoords = HexCoords of int*int
type SquareCoords = SquareCoords of int*int

let fromHex (HexCoords(x, y)) : SquareCoords =
    let y' = y / 2
    SquareCoords(x + y', y)

let toHex (SquareCoords(x, y)) : HexCoords =
    let x' = x - y / 2
    HexCoords(x', y)

let dist (HexCoords(x0, y0) as c0) (HexCoords(x1, y1) as c1) : int =
    let dx = x1 - x0
    let dy = y1 - y0

    if dx < 0 && dy > 0 then
        max -dx dy
    elif dx > 0 && dy < 0 then
        max dx -dy
    else
        abs(dx+dy)
