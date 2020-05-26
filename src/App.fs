module App

open Browser.Dom
open System


///Define my own 2D array since Fable doesn't offer one
type MyArray2D(r, c) =
    //member or even mutable member doesn't seem to allow 
    //setting from Item method, use class local variable
    let _arr = Array.create (r*c) 0.0
    member this.r = r
    member this.c = c
    member this.arr = _arr

    ///Can't override (.[,]) so take a tuple instead
    //member this.(.[]) (i : int * int) = 
    member this.Item
      //with get (i : int * int) : float = arr.[(fst i)*c + (snd i)]
      with get (i : int * int) : float = _arr.[(fst i)*c + (snd i)]
      and set (i : int * int) (value : float) = _arr.[(fst i)*c + (snd i)] <- value

let tau = 2.0 * Math.PI
let frequencies = 4
let coeffs = MyArray2D(frequencies*2, frequencies*2)
let phase = MyArray2D(frequencies*2, frequencies*2)
let rnd = System.Random(42)
for i in -frequencies..frequencies-1 do
    for j in -frequencies..frequencies-1 do
        if i<>0 || j<>0 then
            //coeffs.[(i+frequencies, j+frequencies)] <- (rnd.NextDouble() - 0.5) / float ((i*i + j*j))
            coeffs.[(i+frequencies, j+frequencies)] <- (rnd.NextDouble() - 0.5) /  (float ((i*i + j*j)))
            phase.[(i+frequencies, j+frequencies)] <- (rnd.NextDouble() - 0.5) * tau * 2.0

let speed = 0.05

let WaveHeight x y time =
    let mutable f = 0.0
    let sgn x = if x<0 then -1.0 else 1.0
    for i in -frequencies..frequencies-1 do
        for j in -frequencies..frequencies-1 do
            if i<>0 || j<>0 then
                f <- f + coeffs.[(i+frequencies, j+frequencies)] 
                         * sin(tau * ((x + (sgn i)*speed * time) * float i + (y + (sgn j)*speed * time) * float j) + phase.[(i+frequencies,j+frequencies)])
    f

let Calc1D x y axis time =
    let prescale = 0.8
    let bump = 0.01
    if axis = 0 then
        (WaveHeight (x / prescale) (y / prescale) time) - (WaveHeight (x / prescale + bump) (y / prescale) time)
    else
        (WaveHeight (x / prescale) (y / prescale) time) - (WaveHeight (x / prescale) (y / prescale + bump) time)

let poolHtml time = 
    let h = 20
    let w = 30
    let scale = float (min w h)
    let ds = 1.0 / scale
    let grid = scale * 2.5
    let mutable dx1 = [|for col in 0..w do Calc1D 0.0 (float col / scale) 0 time|]
    let mutable dy1 = [|for col in 0..w do Calc1D 0.0 (float col / scale) 1 time|]
    let svg = 
        [for row in 1..h-1 do
            let rr = float row / scale
            let dy2 = [|for col in 0..w do Calc1D rr (float col / scale) 1 time|]
            let dx2 = [|for col in 0..w do Calc1D rr (float col / scale) 0 time|]
            yield! 
                [for col in 0..w-1 do
                    let debug = row=10 && col=20
                    let cc = float col / scale
                    let xtl = cc    + dx1.[col]
                    let ytl = rr    + dy1.[col]
                    let xtr = cc+ds + dx1.[col+1]
                    let ytr = rr    + dy1.[col+1]
                    let xbl = cc    + dx2.[col]
                    let ybl = rr+ds + dy2.[col]
                    let xbr = cc+ds + dx2.[col+1]
                    let ybr = rr+ds + dy2.[col+1]
                    if debug then
                        printfn "\n%d %d M %f , %f L %f , %f L %f , %f L %f , %f" row col xtl ytl xtr ytr xbr ybr xbl ybl
                    sprintf "M %f,%f L %f,%f L %f,%f L %f,%f" xtl ytl xtr ytr xbr ybr xbl ybl

                    /// map from projected to coords back to screen coords
                    /// formula tested in https://docs.google.com/spreadsheets/d/1dnMlhaIX71SZLfUchIz-cWAUhKnvHQ4G8Uu2TT46g7w/edit#gid=570833227
                    let a,b,c,d,e,f = xtr-xtl,ytr-ytl, xbl-xtl,ybl-ytl, xbr-xtl,ybr-ytl
                    let u,v = e-c-a, f-d-b
                    let screen_coords xx yy = 
                        let x,y = xx-xtl,yy-ytl
                        let A = b*u-a*v
                        let B = b*c-a*d+v*x-u*y
                        let C = d*x-c*y
                        let B2_4AC = B*B - 4.0 * A*C
                        let sx = (-B - sqrt B2_4AC)/(2.0 * A)
                        let sy = (x-a*sx)/(c+u*sx)
                        //assert (B2_4AC>0.0)
                        //if B2_4AC < 0.0 then
                        if debug then
                            printfn "x,y = %f %f sx,sy= %f %f %f %f" x y sx sy ((float col + sx)/scale) ((float row + sy)/scale)
                        ((float col + sx)/scale, (float row + sy)/scale)
                    yield! [
                        // find all full unit squares in the projected from range
                        let minmax a b = min a b |> floor |> int, max a b |> ceil |> int
                        let xtmin, xtmax = minmax (xtl*grid) (xtr*grid)
                        let xbmin, xbmax = minmax (xbl*grid) (xbr*grid)
                        let ylmin, ylmax = minmax (ytl*grid) (ybl*grid)
                        let yrmin, yrmax = minmax (ytr*grid) (ybr*grid)
                        //printfn "%d %d %d %d %d %d %d %d " xtmin xtmax xbmin xbmax ylmin ylmax yrmin yrmax
                        for i in (min xtmin xbmin)..(max xtmax xbmax)-2 do
                          for j in (min ylmin yrmin)..(max ylmax yrmax)-2 do
                            if debug then
                                printfn "a-e u,v= %f %f %f %f %f %f %f %f " a b c d e f u v
                                printfn "i,j= %d %d" i j
                            let (x0,y0),(x1,y1),(x2,y2),(x3,y3) = 
                                (screen_coords (float i/grid)      (float j/grid)),
                                (screen_coords (float (i+1)/grid)  (float j/grid)),
                                (screen_coords (float i/grid)      (float (j+1)/grid)),
                                (screen_coords (float (i+1)/grid)  (float (j+1)/grid))
                            //let check x y = cc<x && x<cc+ds && rr<y && rr<y+ds
                            let check x y = not (Double.IsNaN x) && not (Double.IsNaN y)
                                            && cc<x && x<cc+ds && rr<y && rr<y+ds
                            if check x0 y0 
                                && check x1 y1
                                && check x2 y2
                                && check x3 y3 then
                                sprintf "M %f,%f L %f,%f L %f,%f L %f,%f L %f,%f"
                                    x0 y0 x1 y1 x3 y3 x2 y2 x0 y0
                                if debug then
                                    printfn "xys=  %f %f  %f %f  %f %f  %f %f" x0 y0 x1 y1 x3 y3 x2 y2
                            //sprintf "<rect x="50" y="20" width="150" height="150"
                            //style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9" />                ]
                    ]
                ]
            dx1 <- dx2
            dy1 <- dy2
        ]

    String.concat "\n" (
        [
            "<svg xmlns='http://www.w3.org/2000/svg'"
            "   viewBox='-0.2 -0.2 1.2 1.2' height='100%' width='100%' style='position:absolute'>"
            "<g id='layer1'>"
            "<path ";
            "d='";
        ] @
        svg @ 
        [
            "'";
            //sprintf "transform='translate(%d,%d) scale(1,-1)'" offsetX offsetY
            sprintf "style='fill:none;stroke:#000000;stroke-width:%f'/>" 0.001
            "</g>"
            "</svg>"
        ])

let intervalMs = 100.0
let mutable time = 0.0
let update _ =
    time <- time + intervalMs
    (document.getElementById "pool").innerHTML <- poolHtml (time/1000.0)

update []
let timer = new System.Timers.Timer(intervalMs)
timer.AutoReset <- true
timer.Elapsed.Add update
timer.Start()
