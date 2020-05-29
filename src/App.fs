module App

open Browser.Dom
open System
open System.Diagnostics

open Pool


let intervalMs = 100.0
let mutable time = 0.0
let update _ =
    let start = DateTime.UtcNow.Ticks
    time <- time + intervalMs
    (document.getElementById "pool").innerHTML <- poolHtml (time/1000.0)
    printfn "%d ms" ((DateTime.UtcNow.Ticks-start)/int64 10000)

update []
let timer = new System.Timers.Timer(intervalMs)
timer.AutoReset <- true
timer.Elapsed.Add update
timer.Start()
let keydown _ = 
    if timer.Enabled then
        timer.Stop()
    else
        timer.Start()
document.onkeydown <- keydown