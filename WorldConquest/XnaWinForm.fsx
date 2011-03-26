#r @"C:\Users\johann\Documents\Visual Studio 2010\Projects\WinFormsGraphicsSample_4_0\WinFormsGraphicsDevice\bin\Release\WinFormsGraphicsDevice.exe"

#I @"C:\Program Files (x86)\Microsoft XNA\XNA Game Studio\v4.0\References\Windows\x86"
#r "Microsoft.Xna.Framework.Graphics.dll"
#r "Microsoft.Xna.Framework.dll"
#r "Microsoft.Xna.Framework.Game.dll"

#r @"C:\Users\johann\Documents\xnautils\XNAUtils\bin\Debug\XNAUtils.dll"

#load "PathFinding.fs"
#load "GeneticOptimization.fs"
#load "HexTiling.fs"
#load "Terrain.fs"
#load "Resource.fs"
#load "Regions.fs"
#load "MapCreation.fs"

open System.Windows.Forms
open Microsoft.Xna.Framework

open Terrain
open HexTiling

type XnaControl() =
    inherit WinFormsGraphicsDevice.GraphicsDeviceControl()

    let mutable drawer = fun (dt : GameTime) -> ()
    let watch = new System.Diagnostics.Stopwatch()
    let mutable last_time = watch.Elapsed

    member this.Drawer
        with get ()  = drawer
        and  set (v) = drawer <- v
        
    override this.Initialize() =
        watch.Start()
        last_time <- watch.Elapsed

    override this.Draw() =
        let diff = watch.Elapsed - last_time
        last_time <- watch.Elapsed
        GameTime(diff, watch.Elapsed)
        |> drawer

type XnaForm() =
    inherit Form()

    let ctrl = new XnaControl()
    let animationHandler = new System.EventHandler(fun _ _ -> ctrl.Invalidate())
    do
        ctrl.Dock <- DockStyle.Fill
        base.Controls.Add(ctrl)

    member this.XnaControl = ctrl

    member this.EnableAnimation() =
        Application.Idle.AddHandler(animationHandler)

    member this.DisableAnimation() =
        Application.Idle.RemoveHandler(animationHandler)

let sn x = System.Nullable<_>(x)

let form = new XnaForm()
form.Show()
let content = new Content.ContentManager(form.XnaControl.Services)
content.RootDirectory <- @"C:\Users\johann\Documents\WorldConquest\worldconquest\ContentLibrary\bin\x86\Debug\Content"

let jagged_k = ref 0.75f
let land_mass = ref 0.25f
let terr_size = ref 64

let newTerrain() =
    let heights = makeTerrain !terr_size !terr_size !jagged_k
    let sea_level = findLevel !land_mass heights
    toTerrain sea_level heights

let terr = newTerrain() |> ref
let regions = Regions.markRegions !terr |> ref
let resources = MapCreation.mkResources !terr |> ref

let orig = Vector2.Zero |> ref
let zoom = ref 1.0f

let cursor = ref <| SquareCoords(0, 0)
let path_start : SquareCoords option ref = ref None

let last_mouse_pos = ref None

let mouseDragHandler =
    new MouseEventHandler(fun _ ev ->
        match !last_mouse_pos with
        | None -> ()
        | Some(x, y) ->
            orig := !orig + Vector2(x - ev.X |> float32, y - ev.Y |> float32)
        last_mouse_pos := Some (ev.X, ev.Y)
        form.XnaControl.Invalidate()
    )

form.XnaControl.MouseDown.Add(fun _ ->
    last_mouse_pos := None
    form.XnaControl.MouseMove.AddHandler(mouseDragHandler)
)

form.XnaControl.MouseUp.Add(fun _ ->
    form.XnaControl.MouseMove.RemoveHandler(mouseDragHandler)
)

let (|CursorMoveKey|_|) (k : Keys) =
    match k with
    | Keys.E -> Some 0
    | Keys.R -> Some 1
    | Keys.S -> Some 2
    | Keys.F -> Some 3
    | Keys.X -> Some 4
    | Keys.C -> Some 5
    | _ -> None

form.XnaControl.KeyDown.Add(fun kev ->
    kev.Handled <-
        match kev.KeyCode with
        | Keys.Space ->
            orig := Vector2.Zero
            true
        | Keys.A ->
            zoom := min (!zoom * 1.25f) 2.0f
            true
        | Keys.Z ->
            zoom := max (!zoom / 1.25f) 0.125f
            true
        | Keys.N ->
            terr := newTerrain()
            regions := Regions.markRegions !terr
            resources := MapCreation.mkResources !terr
            true
        | Keys.D ->
            path_start :=
                match !path_start with
                | None -> Some !cursor
                | Some _ -> None
            true
        | CursorMoveKey idx ->
            cursor := neighboursOfSq !cursor |> fun x -> List.nth x idx |> wrapX !terr_size
            true
        | _ -> false

    if kev.Handled then
        form.XnaControl.Invalidate()
)

let units : Graphics.Texture2D = content.Load("units")
let tiles : Graphics.Texture2D = content.Load("tiles")
let font : Graphics.SpriteFont = content.Load("font")

let land_src_rect = new Rectangle(X=86, Y=162, Width=40, Height=48) |> sn
let sea_src_rect = new Rectangle(X=464, Y=162, Width=40, Height=48) |> sn
let oil_src_rect = new Rectangle(X=2, Y=383, Width=40, Height=48) |> sn
let wood_src_rect = new Rectangle(X=380, Y=162, Width=40, Height=48) |> sn
let iron_src_rect = new Rectangle(X=170, Y=235, Width=40, Height=48) |> sn
let factory_src_rect = new Rectangle(X=254, Y=384, Width=40, Height=48) |> sn
let airfield_src_rect = new Rectangle(X=348, Y=108, Width=20, Height=8) |> sn
let harbour_src_rect = new Rectangle(X=128, Y=311, Width=40, Height=48) |> sn
let cursor_src_rect = new Rectangle(X=170, Y=458, Width=40, Height=48) |> sn
let highlight_src_rect = new Rectangle(X=128, Y=381, Width=40, Height=48) |> sn

let batch = new Graphics.SpriteBatch(form.XnaControl.GraphicsDevice)

let getDestPos(x, y) =
    let x =
        if y % 2 = 0 then
            (float32 x) * 40.0f
        else
            (float32 x) * 40.0f + 20.0f

        * !zoom
    let y = (float32 y) * 36.0f * !zoom
            
    new Vector2(x, y) - !orig * !zoom

let getDestRect(dst_pos : Vector2) =
    new Rectangle(X = (dst_pos.X |> int), Y = (dst_pos.Y |> int), Width = (40.0f * !zoom |> int), Height = (48.0f * !zoom |> int))

let drawTile src_rect (x, y) =
    let dst_pos = getDestPos(x, y)
    let dst_rect = getDestRect(dst_pos)
    batch.Draw(tiles, dst_rect, src_rect, Color.White)

type Graphics.SpriteBatch with
    member x.DrawString(font : Graphics.SpriteFont, txt : string, pos : Vector2, color : Color, scale : float32) =
        x.DrawString(font, txt, pos, color, 0.0f, Vector2.Zero, scale, Graphics.SpriteEffects.None, 0.0f)

let drawText (txt : string) (x, y) =
    let text_sz = font.MeasureString(txt)
    let off_x = !zoom * (40.0f - text_sz.X) / 2.0f
    let off_y = !zoom * (48.0f - text_sz.Y) / 2.0f
    let pos = getDestPos(x, y)

    batch.DrawString(font, txt, new Vector2(pos.X + off_x, pos.Y + off_y), Color.Yellow, !zoom)

let drawLand = drawTile land_src_rect

let drawSea = drawTile sea_src_rect

let drawTerrain _ =
    try
        batch.Begin()
        for j in 0..(Array2D.length2 !terr)-1 do
            for i in 0..(Array2D.length1 !terr)-1 do
                match (!terr).[i,j] with
                | Land -> drawLand (i, j)
                | Sea -> drawSea (i, j)
    finally
        batch.End()


let drawRegions _ =
    try
        batch.Begin()
        let region_map, regions = !regions
        let width = getWidth region_map
        let height = getHeight region_map
        for y in 0..height-1 do
            for x in 0..width-1 do
                drawText (region_map.[x, y].ToString()) (x, y)
    finally
        batch.End()

let drawResource rsc_filter src_rect =
    try
        batch.Begin()
        let resources =
            !resources
            |> Array.filter (fun (_, r) -> rsc_filter r)
            |> Seq.map (fun (c, _) -> c)

        for (SquareCoords(x, y)) in resources do
            drawTile src_rect (x, y)
    finally
        batch.End()

let drawOil() =
    drawResource (function Resource.Oil -> true | _ -> false) oil_src_rect                 

let drawWood() =
    drawResource (function Resource.Wood -> true | _ -> false) wood_src_rect

let drawIron() =
    drawResource (function Resource.Iron -> true | _ -> false) iron_src_rect

let drawFactory() =
    drawResource (function Resource.Factory -> true | _ -> false) factory_src_rect

let drawAirfield() =
    drawResource (function Resource.Airfield -> true | _ -> false) airfield_src_rect

let drawHarbour() =
    drawResource (function Resource.Harbour -> true | _ -> false) harbour_src_rect

let drawCursor() =
    let (SquareCoords(x, y)) = !cursor
    try
        batch.Begin()
        drawTile cursor_src_rect (x, y)
    finally
        batch.End()

let drawPath() =
    match !path_start with
    | None -> ()
    | Some start ->
        let path =
            PathFinding.find
                (fun c -> distWrapSq !terr_size !cursor c |> float32)
                (neighboursOfWrapSq !terr_size >> List.filter (Terrain.inRangeSq !terr) >> List.filter (Terrain.getSq !terr >> (=)(Terrain.getSq !terr start)))
                (fun _ -> 1.0f)
                start
        match path with
        | Some coords ->
            try
                batch.Begin()
                for (SquareCoords(x, y)) in coords do
                    drawTile highlight_src_rect (x, y)
            finally
                batch.End()
        | None ->
            ()

form.XnaControl.Drawer <-
    fun _ ->
        drawTerrain()
        drawRegions()
        drawOil()
        drawWood()
        drawIron()
        drawFactory()
        drawAirfield()
        drawHarbour()
        drawCursor()
        drawPath()