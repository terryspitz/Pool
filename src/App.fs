module App

open System
open Fable.Core
open Browser.Types
open Browser.Dom
open Browser.Css
open Browser.CssExtensions   // getComputedStyle
open Pool

type RenderMode =
    | Html      // Draw caustics using SVG
    | Canvas    // Draw caustics onto an HTML Canvas bitmap
    | Gpu       // Draw caustics with a WebGL2 shader

[<Emit("performance.now()")>]
let now () : float = jsNative

[<Emit("new URLSearchParams(window.location.search).get($0)")>]
let queryParam (name: string) : string = jsNative

[<Emit("parseFloat($0)")>]
let parseFloat (s: string) : float = jsNative

/// ?mode=gpu|canvas|svg and ?res=<pixels per grid cell> override the defaults
let requestedMode = queryParam "mode"
let requestedRes =
    let s = queryParam "res"
    if isNull s then None
    else
        let v = parseFloat s
        if Double.IsNaN v || v <= 0. then None else Some v

let pool = document.getElementById "pool"
let canvas = document.getElementById "canvas" :?> HTMLCanvasElement
let stats = document.getElementById "stats"

// A canvas can only ever hand out one kind of context, so pick the renderer
// before asking for one.
let gpu =
    match requestedMode with
    | "canvas" | "svg" | "html" -> None
    | _ -> Gpu.tryCreate canvas

let mode =
    match requestedMode with
    | "svg" | "html" -> Html
    | "canvas" -> Canvas
    | _ -> if gpu.IsSome then Gpu else Canvas

let ctx =
    if mode = Canvas then
        let c = canvas.getContext_2d()
        c.strokeStyle <- U3.Case1 "#000000"
        c
    else
        Unchecked.defaultof<CanvasRenderingContext2D>

let mutable counter = 200
let mutable timeMs = 0.

/// pixels per grid cell. The GPU evaluates the waves per vertex in parallel, so
/// it can afford a much finer grid than the CPU renderer's 10-30. 8 is a
/// conservative default that should hold 60fps on integrated graphics; pass
/// ?res=4 or ?res=2 for a finer mesh.
let defaultRes () =
    match mode with
    | Gpu -> 8.
    | Html -> 1.
    | Canvas ->
        // resolution varies fast/slow
        if counter % 300 < 250 then 30. else 10.

// stats are accumulated and shown about twice a second: the original per-frame
// printfn was itself a measurable share of the frame time
let mutable framesSinceStats = 0
let mutable renderMsSinceStats = 0.
let mutable lastStatsAt = now ()

let showStats () =
    let elapsed = now () - lastStatsAt
    if elapsed > 500. && framesSinceStats > 0 then
        let perFrame = renderMsSinceStats / float framesSinceStats
        let fps = 1000. * float framesSinceStats / elapsed
        let where =
            match mode with
            | Gpu -> sprintf "webgl2, %d verts, %d waves" gpu.Value.VertexCount gpu.Value.WaveCount
            | Canvas -> "2d canvas"
            | Html -> "svg"
        stats.innerHTML <- sprintf "%.1f fps &middot; %.1f ms/frame &middot; %s" fps perFrame where
        framesSinceStats <- 0
        renderMsSinceStats <- 0.
        lastStatsAt <- now ()

let update () =
    let start = now ()
    counter <- counter + 1
    let res = match requestedRes with Some r -> r | None -> defaultRes ()
    timeMs <- timeMs + 2.
    match mode with
    | Html ->
        pool.innerHTML <- String.concat "\n" (poolHtml timeMs)
    | Gpu ->
        gpu.Value.Draw(timeMs, res)
    | Canvas ->
        ctx.setTransform(1., 0., 0., 1., 0., 0.)
        ctx.scale(canvas.height, canvas.height)
        ctx.fillStyle <- U3.Case1 "#146897" //from pool.jpg
        ctx.fillRect(0., 0., canvas.width, canvas.height)
        drawCaustics ctx timeMs res
    renderMsSinceStats <- renderMsSinceStats + (now () - start)
    framesSinceStats <- framesSinceStats + 1
    showStats ()

let mutable timerOn = true

let rec animate (dt:float) =
    if timerOn then
        window.requestAnimationFrame(animate) |> ignore
        if window.getComputedStyle(canvas).opacity <> "0" then
            update ()

let resize(_) =
    let scale = 1.
    canvas.width <- window.innerWidth / scale
    canvas.height <- window.innerHeight / scale
    if scale <> 1. then
        canvas.setAttribute ("style", sprintf "transform-origin: 0 0; transform: scale(%d)" (int scale))
    update ()

let flipTimer _ =
    timerOn <- not timerOn
    animate 0.

// document.body.onkeydown <- flipTimer
document.body.onclick <- flipTimer
window.onresize <- resize

resize()
update ()
animate 0.
