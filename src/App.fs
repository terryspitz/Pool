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
let mutable qualityMultiplier = 1.0
let pool = document.getElementById "pool"
let canvas = document.getElementById "canvas" :?> HTMLCanvasElement
let ctx = canvas.getContext_2d()
ctx.strokeStyle <- U3.Case1 "#000000"

let update() =
    let start = DateTime.UtcNow.Ticks
    counter <- counter + 1
    // resolution varies fast/slow
    let res =
        (if counter % 300 < 250 then
            30.
        else
            10.) / qualityMultiplier
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
        ctx.fillText("Click the pool, or press space, to pause/play.", 0., 20.)

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

// UI chrome: play/pause, fullscreen and a settings panel of sliders.
let playToggleButton = document.getElementById "play-toggle" :?> HTMLButtonElement
let fullscreenButton = document.getElementById "fullscreen-toggle" :?> HTMLButtonElement
let settingsToggleButton = document.getElementById "settings-toggle" :?> HTMLButtonElement
let settingsPanel = document.getElementById "settings-panel"
let brightnessSlider = document.getElementById "brightness-slider" :?> HTMLInputElement
let speedSlider = document.getElementById "speed-slider" :?> HTMLInputElement
let amplitudeSlider = document.getElementById "amplitude-slider" :?> HTMLInputElement
let qualitySlider = document.getElementById "quality-slider" :?> HTMLInputElement

let togglePlay _ =
    timerOn <- not timerOn
    playToggleButton.innerHTML <- (if timerOn then "&#10074;&#10074;" else "&#9658;")
    animate 0.

let toggleFullscreen _ =
    if isNull document.fullscreenElement then
        document.documentElement.requestFullscreen() |> ignore
    else
        document.exitFullscreen() |> ignore

let toggleSettings _ =
    settingsPanel.classList.toggle "open" |> ignore

// The buttons and settings panel are separate elements layered on top of
// the canvas, so clicks on them never reach canvas.onclick below.
playToggleButton.onclick <- togglePlay
fullscreenButton.onclick <- toggleFullscreen
settingsToggleButton.onclick <- toggleSettings

let refreshIfPaused () =
    if not timerOn then update ()

brightnessSlider.oninput <- fun _ ->
    Pool.brightness <- float brightnessSlider.value
    refreshIfPaused ()
speedSlider.oninput <- fun _ ->
    Pool.waveSpeed <- float speedSlider.value
    refreshIfPaused ()
amplitudeSlider.oninput <- fun _ ->
    Pool.waveAmplitude <- float amplitudeSlider.value
    refreshIfPaused ()
qualitySlider.oninput <- fun _ ->
    qualityMultiplier <- float qualitySlider.value
    refreshIfPaused ()

canvas.onclick <- togglePlay
window.onresize <- resize

document.body.onkeydown <- fun e ->
    let active = document.activeElement
    let focusInInput = not (isNull active) && active.tagName = "INPUT"
    if e.key = " " && not focusInInput then
        e.preventDefault()
        togglePlay ()

resize()
update ()
animate 0.
