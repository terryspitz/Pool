module App

open Browser.Dom
open Pool

// Renders the caustics as SVG, mapping the refracted pool floor back to screen
// coordinates a tile at a time. See Pool.poolHtml and the Point/Poly clipping
// it is built on.

let pool = document.getElementById "pool"

let mutable timeMs = 0.

let update () =
    timeMs <- timeMs + 2.
    pool.innerHTML <- String.concat "\n" (poolHtml timeMs)

let rec animate (_: float) =
    window.requestAnimationFrame(animate) |> ignore
    update ()

update ()
animate 0.
