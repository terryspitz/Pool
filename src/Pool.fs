module Pool

open System
open System.Diagnostics


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
      with get (i : int * int) : float =
        assert ((fst i)<r && (snd i)<c)
        _arr.[(fst i)*c + (snd i)]
      and set (i : int * int) (value : float) =
        assert ((fst i)<r && (snd i)<c)
        _arr.[(fst i)*c + (snd i)] <- value

let tau = 2.0 * Math.PI
let frequencies = 5
let coeffs = MyArray2D(frequencies*2+1, frequencies*2+1)
let phase = MyArray2D(frequencies*2+1, frequencies*2+1)
let rnd = System.Random(42)  // seeds not supported by Fable or js
for i in -frequencies..frequencies do
    for j in -frequencies..frequencies do
        if i<>0 || j<>0 then
            coeffs.[(i+frequencies, j+frequencies)] <- (rnd.NextDouble() - 0.5) /  (float ((i*i + j*j)))
            phase.[(i+frequencies, j+frequencies)] <- rnd.NextDouble() * tau


let Calc1D x y axis time =
    let speed = 0.05
    let scaling = 0.05
    let sgn x = if x<0 then -1.0 else 1.0
    let mutable f = 0.0
    if axis = 0 then
        for i in -frequencies..frequencies do
            let iSign = float (Math.Sign i)
            for j in -frequencies..frequencies do
                let jSign = float (Math.Sign j)
                let amp, phase = coeffs.[(i+frequencies, j+frequencies)], phase.[(i+frequencies,j+frequencies)]
                f <- f + amp * (float i) * cos (Math.PI * ((x+iSign*speed*time) * float i + (y+jSign*speed*time) * float j) + phase)
     elif axis = 1 then
        for i in -frequencies..frequencies do
            let iSign = float (Math.Sign i)
            for j in -frequencies..frequencies do
                let jSign = float (Math.Sign j)
                let amp, phase = coeffs.[(i+frequencies, j+frequencies)], phase.[(i+frequencies,j+frequencies)]
                f <- f + amp * (float j) * cos (Math.PI * ((x+iSign*speed*time) * float i + (y+jSign*speed*time) * float j) + phase)
    f*scaling

[<Struct>]
type Point = { X: float; Y: float }
with
    override this.ToString() = sprintf "%.4f,%.4f" this.X this.Y
    static member (+) (p1, p2) = {X=p1.X+p2.X; Y=p1.Y+p2.Y}
    ///rotates by 90deg clockwise about (0.5,0.5)
    member this.Rotate90C = {X=this.Y; Y=1.0-this.X}
    member this.PosX = this.X > 0.0

type Poly = { pts: Point[] }
with
    member this.Rotate90C = {pts=[|for p in this.pts do p.Rotate90C|]}
    member this.CropX =
        match this.pts |> Array.tryFindIndex (fun p -> p.PosX) with
        | None -> {pts=[||]}
        | Some firstIn ->
            let n = this.pts.Length
            {pts=Array.ofList [
                for i in firstIn..firstIn+n-1 do
                    let p, q = this.pts.[i%n], this.pts.[(i+1)%n]
                    if p.PosX && q.PosX then
                        yield q
                    elif p.PosX then
                        yield {X=0.0; Y=p.Y + (q.Y-p.Y)*p.X/(p.X-q.X)}
                    elif q.PosX then
                        yield {X=0.0; Y=p.Y + (q.Y-p.Y)*p.X/(p.X-q.X)}
                        yield q
            ]}
    member this.CropUnitSquare =
        let mutable p = this
        for i in 0..3 do
            p <- p.CropX.Rotate90C
        if p.pts.Length >0 then
            Debug.Assert(((Array.map (fun p->p.X) p.pts |> Array.min) >= 0.0), (sprintf "pts: %A, p: %A" this.pts p))
            Debug.Assert(((Array.map (fun p->p.Y) p.pts |> Array.min) >= 0.0), (sprintf "pts: %A, p: %A" this.pts p))
            Debug.Assert(((Array.map (fun p->p.Y) p.pts |> Array.max) <= 1.0), (sprintf "pts: %A, p: %A" this.pts p))
            Debug.Assert(((Array.map (fun p->p.X) p.pts |> Array.max) <= 1.0), (sprintf "pts: %A, p: %A" this.pts p))
        p
    static member (+) (poly,pt) = {pts=Array.map (fun p -> p+pt) poly.pts}
    member this.ToSvg =
        if this.pts.Length>0 then
            sprintf "M %s L " (this.pts.[0].ToString())
            + String.concat " L " [for i in 1..this.pts.Length do this.pts.[i%this.pts.Length].ToString()]
        else
            ""

let poolHtml time = 
    let w = 16
    let h = 9
    let scale = float (min w h)
    let ds = 1.0 / scale
    let grid = scale * 4.0
    let mutable dx1 = [|for col in 0..w do Calc1D 0.0 (float col / scale) 0 time|]
    let mutable dy1 = [|for col in 0..w do Calc1D 0.0 (float col / scale) 1 time|]
    let svg = 
        [for row in 0..h-1 do
            let rr = float row / scale
            let dx2 = [|for col in 0..w do Calc1D (rr+ds) (float col / scale) 0 time|]
            let dy2 = [|for col in 0..w do Calc1D (rr+ds) (float col / scale) 1 time|]
            for col in 0..w-1 do
                let debug = row=0 && col=0
                // let debug = false
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
                //sprintf "M %f,%f L %f,%f L %f,%f L %f,%f" xtl ytl xtr ytr xbr ybr xbl ybl

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
                    {X=sx; Y=sy}
                // find all full unit squares in the projected from range
                let minmax a b = min a b |> floor |> int, max a b |> ceil |> int
                let xtmin, xtmax = minmax (xtl*grid) (xtr*grid)
                let xbmin, xbmax = minmax (xbl*grid) (xbr*grid)
                let ylmin, ylmax = minmax (ytl*grid) (ybl*grid)
                let yrmin, yrmax = minmax (ytr*grid) (ybr*grid)
                //printfn "%d %d %d %d %d %d %d %d " xtmin xtmax xbmin xbmax ylmin ylmax yrmin yrmax
                for i in (min xtmin xbmin)..(max xtmax xbmax)-1 do
                  for j in (min ylmin yrmin)..(max ylmax yrmax)-1 do
                    if i%5=2 || j%5=2 then
                        if debug then
                            printfn "a-e u,v= %f %f %f %f %f %f %f %f " a b c d e f u v
                            printfn "i,j= %d %d" i j
                        let poly = {pts=[|
                            (screen_coords (float i/grid)      (float j/grid))
                            (screen_coords (float (i+1)/grid)  (float j/grid))
                            (screen_coords (float (i+1)/grid)  (float (j+1)/grid))
                            (screen_coords (float i/grid)      (float (j+1)/grid))
                        |]}
                        let check p = not (Double.IsNaN p.X) && not (Double.IsNaN p.Y)
                        if Array.fold (fun s p -> s&&check p) true poly.pts then
                            let poly = poly.CropUnitSquare
                            let s0 = {X=float col; Y=float row}
                            if debug then
                                printfn "poly= %A" (poly+s0).ToSvg
                            //sprintf "<rect x="50" y="20" width="150" height="150"
                            //style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9" />                ]
                            yield (poly+s0).ToSvg
            dx1 <- dx2
            dy1 <- dy2
        ]

    String.concat "\n" (
        [
            "<svg xmlns='http://www.w3.org/2000/svg'"
            sprintf "   viewBox='0 0 %d %d'>" w h
            // sprintf "   viewBox='-0.5 -0.5 %d.5 %d.5' style='position: relative; top: 50%%; left: 50%%; transform: translate(-50%%, -50%%);'>" (w+1) (h+1)
            "<g id='layer1'>"
            "<path ";
            "d='";
        ] @
        svg @ 
        [
            "'";
            //sprintf "transform='translate(%d,%d) scale(1,-1)'" offsetX offsetY
            sprintf "style='fill:#000000'/>"
            //sprintf "style='fill:none;stroke:#000000;stroke-width:%f'/>" 0.001
            "</g>"
            "</svg>"
        ])
