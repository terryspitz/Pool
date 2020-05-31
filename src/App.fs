module App

open Browser.Dom
open System
open System.Diagnostics
open Browser.Types
open Fable.Core
open Browser

open Pool

let drawCaustics (ctx : CanvasRenderingContext2D) time = 
    let mutable minmaxarea = (1.,0.) 
    let res = 15.
    let w = int (ctx.canvas.width / res)
    let h = int (ctx.canvas.height / res)
    let scale = float (min w h)
    let ds = 1. / float scale
    let margin = 5
    let mutable dx1 = [|for col in -margin..w+margin do Calc1D 0. (float col / scale) 0 time|]
    let mutable dy1 = [|for col in -margin..w+margin do Calc1D 0. (float col / scale) 1 time|]
    for row in -margin..h+margin-1 do
        let rr = float row / scale
        let dx2 = [|for col in -margin..w+margin do Calc1D (rr+ds) (float col / scale) 0 time|]
        let dy2 = [|for col in -margin..w+margin do Calc1D (rr+ds) (float col / scale) 1 time|]
        for col in -margin..w+margin-1 do
            let cc = float col / scale
            let xtl = cc    + dx1.[col]
            let ytl = rr    + dy1.[col]
            let xtr = cc+ds + dx1.[col+1]
            let ytr = rr    + dy1.[col+1]
            let xbl = cc    + dx2.[col]
            let ybl = rr+ds + dy2.[col]
            let xbr = cc+ds + dx2.[col+1]
            let ybr = rr+ds + dy2.[col+1]
            let cross x1 y1 x2 y2 = abs (x1*y2-x2*y1)
            let area = (cross (xtr-xtl) (ytr-ytl) (xbl-xtl) (ybl-ytl) + cross (xtr-xbr) (ytr-ybr) (xbl-xbr) (ybl-ybr) ) / 2. * scale * scale
            ctx.beginPath()
            ctx.moveTo(xtl, ytl)
            let offset = -0.001
            ctx.lineTo(xtr-offset, ytr)
            ctx.lineTo(xbr-offset, ybr-offset)
            ctx.lineTo(xbl, ybl-offset)
            ctx.closePath()
            let alpha = min (0.6/area) 1.0
            let colour = sprintf "rgba(255,255,255,%f)" alpha
            ctx.fillStyle <- U3.Case1 colour
            // printfn "%A" colour
            ctx.fill()
            minmaxarea <- (min alpha (fst minmaxarea), max alpha (snd minmaxarea))
    
        dx1 <- dx2
        dy1 <- dy2

    printfn "%A" minmaxarea


let intervalMs = 0.08
let mutable timeMs = 0.
let pool = document.getElementById "pool"
let canvas = document.getElementById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
ctx.strokeStyle <- U3.Case1 "#000000"

let update() =
    let start = DateTime.UtcNow.Ticks
    timeMs <- timeMs + intervalMs
    // pool.innerHTML <- poolHtml (time/1000.)
    canvas.width <- window.innerWidth
    canvas.height <- window.innerHeight
    let ctx = canvas.getContext_2d()
    ctx.setTransform(1., 0., 0., 1., 0., 0.)
    ctx.scale(canvas.height, canvas.height)
    ctx.fillStyle <- U3.Case1 "#146897" //from pool.jpg
    ctx.fillRect(0., 0., canvas.width, canvas.height)
    drawCaustics ctx timeMs

    printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/10000L)

let mutable timerOn = true

let rec animate (dt:float) =
    if timerOn then
        window.requestAnimationFrame(animate) |> ignore
        update ()

let flipTimer _ =
    timerOn <- not timerOn
    animate 0.
document.onkeydown <- flipTimer
document.onclick <- flipTimer

update ()
animate 0.
