module HexTiling

open System.Collections.Generic

type HexCoords = HexCoords of int*int
type SquareCoords = SquareCoords of int*int

let fromHex (HexCoords(x, y)) : SquareCoords =
    let y' = y >>> 1
    SquareCoords(x + y', y)

let toHex (SquareCoords(x, y)) : HexCoords =
    let x' = x - (y >>> 1)
    HexCoords(x', y)

let distDx getDx (HexCoords(x0, y0) as c0) (HexCoords(x1, y1) as c1) : int =
    let dx = getDx x0 x1
    let dy = y1 - y0

    if dx < 0 && dy > 0 then
        max -dx dy
    elif dx > 0 && dy < 0 then
        max dx -dy
    else
        abs(dx+dy)

let dist = distDx (fun x0 x1 -> x1 - x0)

let wrap w x0 x1 =
    let w2 = w / 2
    let dx = x1 - x0
    if dx > w2 then dx - w
    elif dx < -w2 then dx + w
    else dx

let distWrap w = distDx (wrap w)

let distSq (c0 : SquareCoords) (c1 : SquareCoords) : int =
    dist (toHex c0) (toHex c1)

let distWrapSq w c0 c1 = distWrap w (toHex c0) (toHex c1)

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

let wrapX w (SquareCoords(x, y)) =
    let x =
        if x < 0 then x + w
        elif x >= w then x - w
        else x
    SquareCoords(x, y)

let neighboursOfWrapSq w c0 =
    neighboursOfSq c0
    |> List.map (wrapX w)

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

let cachedWithin = new Dictionary<int, HexCoords[]>()

let getAllWithin n =
    match cachedWithin.TryGetValue(n) with
    | true, v -> v
    | false, _ ->
        let v =
            [|
                let c0 = HexCoords(0, 0)
                for i in -n..n do
                    for j in -n..n do
                        let c = toHex(SquareCoords(i, j))
                        if dist c c0 <= n then
                            yield c
            |]
        cachedWithin.Add(n, v)
        v

do for i in 0..20 do getAllWithin i |> ignore