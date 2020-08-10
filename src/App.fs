module App

open System
open System.Diagnostics
open Fable.Core
open Browser.Types
open Browser.Dom

open Pool


let intervalMs = 0.08
let mutable timeMs = 0.
let pool = document.getElementById "pool"
let canvas = document.getElementById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
ctx.strokeStyle <- U3.Case1 "#000000"

type HtmlOrCanvas = 
    | Html | Canvas

let update() =
    let htmlOrCanvas = Canvas
    let start = DateTime.UtcNow.Ticks
    timeMs <- timeMs + intervalMs
    if htmlOrCanvas = Html then
        pool.innerHTML <- String.concat "\n" (poolHtml timeMs)
    canvas.width <- window.innerWidth
    canvas.height <- window.innerHeight
    let ctx = canvas.getContext_2d()
    ctx.setTransform(1., 0., 0., 1., 0., 0.)
    if htmlOrCanvas = Html then
        ctx.scale(100., 100.)
    else
        ctx.scale(canvas.height, canvas.height)
    ctx.fillStyle <- U3.Case1 "#146897" //from pool.jpg
    ctx.fillRect(0., 0., canvas.width, canvas.height)
    if htmlOrCanvas = Canvas then
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
document.body.onkeydown <- flipTimer
document.body.onclick <- flipTimer

update ()
animate 0.
