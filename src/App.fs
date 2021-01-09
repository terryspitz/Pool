module App

open System
open System.Diagnostics
open Fable.Core
open Browser.Types
open Browser.Dom
open Browser.Css
open Browser.CssExtensions
open Pool

type HtmlOrCanvas = 
    | Html      // Draw caustics using SVG
    | Canvas    // Draw caustics onto an HTML Canvas bitmap

let htmlOrCanvasMode = Canvas
let mutable timeMs = 0.
let mutable counter = 200
let pool = document.getElementById "pool"
let canvas = document.getElementById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
ctx.strokeStyle <- U3.Case1 "#000000"

let update() =
    let start = DateTime.UtcNow.Ticks
    counter <- counter + 1
    // resolution varies fast/slow
    let res = 
        if counter % 300 < 250 then
            30.
        else
            10.
    timeMs <- timeMs + 2.
    // printfn "res %f" res
    match htmlOrCanvasMode with
    | Html ->
        pool.innerHTML <- String.concat "\n" (poolHtml timeMs)
    | Canvas ->
        ctx.setTransform(1., 0., 0., 1., 0., 0.)
        ctx.scale(canvas.height, canvas.height)
        ctx.fillStyle <- U3.Case1 "#146897" //from pool.jpg
        ctx.fillRect(0., 0., canvas.width, canvas.height)
        drawCaustics ctx timeMs res

        ctx.setTransform(1., 0., 0., 1., 0., 0.)
        ctx.fillStyle <- U3.Case1 "black"
        ctx.font <- "20px Georgia"
        ctx.fillText("Click anywhere to stop/start animation.", 0., 20.)

    printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/10000L)

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
