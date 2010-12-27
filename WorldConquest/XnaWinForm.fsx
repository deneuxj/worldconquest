#r @"D:\Documents\WinForms\WinFormsGraphicsDevice\bin\Release\WinFormsGraphicsDevice.exe"

#I @"C:\Program Files\Microsoft XNA\XNA Game Studio\v4.0\References\Windows\x86"
#r "Microsoft.Xna.Framework.Graphics.dll"
#r "Microsoft.Xna.Framework.dll"
#r "Microsoft.Xna.Framework.Game.dll"

#load "GeneticOptimization.fs"
#load "HexTiling.fs"
#load "Terrain.fs"
#load "Resource.fs"

open System.Windows.Forms
open Microsoft.Xna.Framework
open Terrain

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
content.RootDirectory <- @"D:\Documents\WorldConquest\ContentLibrary\bin\x86\Debug\Content"

let newTerrain() =
    let jagged_k = 0.5f
    let land_mass = 0.25f

    let heights = makeTerrain 64 64 jagged_k
    let sea_level = findLevel land_mass heights
    toTerrain sea_level heights

let terr = newTerrain() |> ref
let orig = Vector2.Zero |> ref

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

form.XnaControl.KeyDown.Add(fun kev ->
    kev.Handled <-
        match kev.KeyCode with
        | Keys.Space ->
            orig := Vector2.Zero
            true
        | Keys.N ->
            terr := newTerrain()
            true
        | _ -> false

    if kev.Handled then
        form.Invalidate()
)

let units : Graphics.Texture2D = content.Load("units")
let tiles : Graphics.Texture2D = content.Load("tiles")
let land_src_rect = new Rectangle(X=86, Y=162, Width=40, Height=48) |> sn
let sea_src_rect = new Rectangle(X=464, Y=162, Width=40, Height=48) |> sn
 
let batch = new Graphics.SpriteBatch(form.XnaControl.GraphicsDevice)

let drawTile src_rect orig (x, y) =
    let x =
        if y % 2 = 0 then
            x * 40
        else
            x * 40 + 20
        |> float32

    let y = y * 36 |> float32
            
    let dst_pos = new Vector2(x, y) - orig
    batch.Draw(tiles, dst_pos, src_rect, Color.White)

let drawLand = drawTile land_src_rect

let drawSea = drawTile sea_src_rect

let drawTerrain _ =
    try
        batch.Begin()
        for j in 0..(Array2D.length2 !terr)-1 do
            for i in 0..(Array2D.length1 !terr)-1 do
                match (!terr).[i,j] with
                | Land -> drawLand !orig (i, j)
                | Sea -> drawSea !orig (i, j)
    finally
        batch.End()

form.XnaControl.Drawer <- drawTerrain
