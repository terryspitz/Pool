module App

open Browser.Dom
open System
open System.Diagnostics
open Browser.Types
open Fable.Core
open Browser

open Pool

let drawCaustics (ctx : CanvasRenderingContext2D) time = 
    let debug = false
    let mutable minmaxarea = (1.,0.) 
    let res = 10.
    let w = int (ctx.canvas.width / res)
    let h = int (ctx.canvas.height / res)
    let scale = float (min w h)
    let ds = 1. / float scale
    let margin = 5
    let mutable derivs1 = Array.create (w+margin*2) (0.,0.)
    let mutable derivs2 = Array.create (w+margin*2) (0.,0.)
    for col in -margin..w+margin do
        derivs1.[col+margin] <- CalcDerivs (float col * ds) 0. time
    for row in -margin..h+margin-1 do
        let py = float row * ds
        for col in -margin..w+margin do
            derivs2.[col+margin] <- CalcDerivs (float col * ds) (py+ds) time
        for col in -margin..w+margin-1 do
            let px = float col * ds
            let xtl = px    + fst derivs1.[col+margin]
            let ytl = py    + snd derivs1.[col+margin]
            let xtr = px+ds + fst derivs1.[col+margin+1]
            let ytr = py    + snd derivs1.[col+margin+1]
            let xbl = px    + fst derivs2.[col+margin]
            let ybl = py+ds + snd derivs2.[col+margin]
            let xbr = px+ds + fst derivs2.[col+margin+1]
            let ybr = py+ds + snd derivs2.[col+margin+1]
            // printfn "\n%d %d M %f , %f L %f , %f L %f , %f L %f , %f" row col xtl ytl xtr ytr xbr ybr xbl ybl

            let cross x1 y1 x2 y2 = abs (x1*y2-x2*y1)
            let area = (cross (xtr-xtl) (ytr-ytl) (xbl-xtl) (ybl-ytl) + cross (xtr-xbr) (ytr-ybr) (xbl-xbr) (ybl-ybr) ) / 2. * scale * scale
            ctx.beginPath()
            ctx.moveTo(xtl, ytl)
            let offset = -0.000
            ctx.lineTo(xtr-offset, ytr)
            ctx.lineTo(xbr-offset, ybr-offset)
            ctx.lineTo(xbl, ybl-offset)
            ctx.closePath()
            let alpha = min (0.6/area) 1.0
            let colour = sprintf "rgba(255,255,255,%f)" alpha
            ctx.fillStyle <- U3.Case1 colour
            if debug then
                printfn "%A" colour
            ctx.fill()
            if debug then
                minmaxarea <- (min alpha (fst minmaxarea), max alpha (snd minmaxarea))
    
        for col in -margin..w+margin do
            derivs1.[col+margin] <- derivs2.[col+margin]

    if debug then
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
    // pool.innerHTML <- String.concat "\n" (poolHtml timeMs)
    canvas.width <- window.innerWidth
    canvas.height <- window.innerHeight
    let ctx = canvas.getContext_2d()
    ctx.setTransform(1., 0., 0., 1., 0., 0.)
    ctx.scale(canvas.height, canvas.height)
    ctx.fillStyle <- U3.Case1 "#146897" //from pool.jpg
    ctx.fillRect(0., 0., canvas.width, canvas.height)
    drawCaustics ctx timeMs

    printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/10000L)

let mutable timerOn = false

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
