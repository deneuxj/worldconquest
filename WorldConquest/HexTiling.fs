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

let neighboursOfSq (SquareCoords(i, j)) =
    let i0 = i-1
    let i1 = i
    let i2 = i+1
    let j0 = j-1
    let j1 = j
    let j2 = j+1

    if j % 2 = 0 then
        [
             (i0, j0); (i1, j0);
          (i0, j1);       (i2, j1);
             (i0, j2); (i1, j2)
        ]
    else
        [
             (i1, j0); (i2, j0);
          (i0, j1);       (i2, j1);
             (i1, j2); (i2, j2)
        ]
    |> List.map SquareCoords

let neighboursOf (HexCoords(i, j)) =
    let i0 = i-1
    let i1 = i
    let i2 = i+1
    let j0 = j-1
    let j1 = j
    let j2 = j+1

    [
            (i1, j0); (i2, j0);
        (i0, j1);       (i2, j1);
            (i0, j2); (i1, j2)
    ]
    |> List.map HexCoords