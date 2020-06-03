module Pool

open System
open System.Diagnostics

open MyArray2D

let tau = 2.0 * Math.PI
let frequencies = 11
let coeffs = MyArray2D(frequencies*2+1, frequencies*2+1)
let phase = MyArray2D(frequencies*2+1, frequencies*2+1)
let rnd = System.Random(42)  // seeds not supported by Fable or js
for i in -frequencies..frequencies do
    for j in -frequencies..frequencies do
        if i<>0 || j<>0 then
            coeffs.[(i+frequencies, j+frequencies)] <- (rnd.NextDouble() - 0.5) /  (float ((i*i + j*j)))
            phase.[(i+frequencies, j+frequencies)] <- rnd.NextDouble() * tau

/// https://gist.github.com/geraldyeo/988116 simplified
let fastCos x = 
    let mutable x = x / 3.14159265 + 1.5
    let xx = 2. * floor (x / 2.)
    x <- x - xx - 1.
    assert (-1.<=x && x<1.)
    if x < 0. then
        4. * (1.+x) * x
    else
        4. * (1.-x) * x

let CalcDerivs x y time =
    let speed = 0.05
    let scaling = 0.08
    let sgn x = if x<0 then -1. else 1.
    let mutable dx, dy = 0., 0.
    for i in -frequencies..frequencies do
        let iSign = sgn i
        for j in -frequencies..frequencies do
            let jSign = sgn j
            let amp, phase = coeffs.[(i+frequencies, j+frequencies)], phase.[(i+frequencies,j+frequencies)]
            let costheta = fastCos (Math.PI * ((x+iSign*speed*time) * float i + (y+jSign*speed*time) * float j) + phase)
            dx <- dx + amp * (float i) * costheta
            dy <- dy + amp * (float j) * costheta
    dx*scaling, dy*scaling

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
    member this.ToSvg colour =
        if this.pts.Length>0 then
            [
                "<path "
                "d='"
                sprintf "M %s L " (this.pts.[0].ToString())
                + String.concat " L " [for i in 1..this.pts.Length do this.pts.[i%this.pts.Length].ToString()]
                "'"
                if colour = "none" then
                    sprintf "style='fill:none'/>"
                    // sprintf "style='fill:none;stroke:#000000;stroke-width:0.01'/>"
                elif colour <> "000000" then 
                    sprintf "style='fill:#%s'/>" colour
                else 
                    "/>"
                //sprintf "style='fill:none;stroke:#000000;stroke-width:%f'/>" 0.001
            ]
        else
            []
    static member UnitSquare = {pts=[|{X=0.0;Y=0.0}; {X=0.0;Y=1.0}; {X=1.0;Y=1.0}; {X=1.0;Y=0.0}|]}
    static member NearUnitSquare = {pts=[|{X=0.1;Y=0.1}; {X=0.1;Y=0.9}; {X=0.9;Y=0.9}; {X=0.9;Y=0.1}|]}

let poolHtml time = 
    let mult = 1
    let w = 16 * mult
    let h = 9 * mult
    let scale = float (min w h)
    let ds = 1.0 / scale
    let grid = scale * 2.0
    let mutable derivs1 = Array.create (w+1) (0., 0.)
    let mutable derivs2 = Array.create (w+1) (0., 0.)
    for col in 0..w do
        derivs1.[col] <- CalcDerivs 0. (float col * ds) time
    let svg = 
        [for row in 0..h-1 do
            let rr = float row * ds
            for col in 0..w do
                derivs2.[col] <- CalcDerivs (rr+ds) (float col * ds) time
            for col in 0..w-1 do
                let debug = row=0 && col=0
                let debug = false
                let cc = float col * ds
                let xtl = cc    + fst derivs1.[col]
                let ytl = rr    + snd derivs1.[col]
                let xtr = cc+ds + fst derivs1.[col+1]
                let ytr = rr    + snd derivs1.[col+1]
                let xbl = cc    + fst derivs2.[col]
                let ybl = rr+ds + snd derivs2.[col]
                let xbr = cc+ds + fst derivs2.[col+1]
                let ybr = rr+ds + snd derivs2.[col+1]
                if debug then
                    printfn "\n%d %d M %f , %f L %f , %f L %f , %f L %f , %f" row col xtl ytl xtr ytr xbr ybr xbl ybl
                let simple = true
                let spiro = true
                if simple then
                    let poly = {pts=[|{X=xtl*scale;Y=ytl*scale}; {X=xtr*scale;Y=ytr*scale};
                                {X=xbr*scale;Y=ybr*scale}; {X=xbl*scale;Y=ybl*scale}|]}
                    yield! poly.ToSvg (if ((col+5)%(w/4))<2 then "4444ff" else "none")
                // elif spiro then
                //     yield! poly.ToSvg (if ((col+5)%(w/4))<2 then "4444ff" else "none")

                else
                    /// map from projected to coords back to screen coords
                    /// i.e. from refracted bottom of pool to surface
                    /// formula tested in https://docs.google.com/spreadsheets/d/1dnMlhaIX71SZLfUchIz-cWAUhKnvHQ4G8Uu2TT46g7w/edit#gid=570833227
                    let a,b,c,d,e,f = xtr-xtl,ytr-ytl, xbl-xtl,ybl-ytl, xbr-xtl,ybr-ytl
                    let u,v = e-c-a, f-d-b
                    let screen_coords xx yy = 
                        let x,y = xx-xtl,yy-ytl
                        let A = b*u-a*v
                        let B = b*c-a*d+v*x-u*y
                        let C = d*x-c*y
                        let B2_4AC = B*B - 4.0 * A*C
                        // let sqrtB2_4AC = sqrt (max B2_4AC 0.0)
                        let sqrtB2_4AC = sqrt B2_4AC
                        let sx = (-B - sqrtB2_4AC)/(2.0 * A)
                        let sy = (x-a*sx)/(c+u*sx)
                        //assert (B2_4AC>0.0)
                        //if B2_4AC < 0.0 then
                        if debug then
                            printfn "x,y = %f %f sx,sy= %f %f , %f" x y sx sy sqrtB2_4AC
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
                        //floor pattern
                        if ((i+5)%(w/2))<5 then
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
                            let s0 = {X=float col; Y=float row}
                            if Array.fold (fun s p -> s&&check p) true poly.pts then
                                let poly = poly.CropUnitSquare
                                if debug then
                                    printfn "poly= %A" ((poly+s0).ToSvg "000000")
                                //sprintf "<rect x="50" y="20" width="150" height="150"
                                //style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9" />                ]
                                yield! (poly+s0).ToSvg "4444ff"
                            // else
                            //     yield! (Poly.NearUnitSquare+s0).ToSvg "ff0000"
            for col in 0..w do
                derivs1.[col] <- derivs2.[col]
        ]

    [
        "<svg xmlns='http://www.w3.org/2000/svg'"
        sprintf "   viewBox='0 0 %d %d'>" w h
        // sprintf "   viewBox='-0.5 -0.5 %d.5 %d.5' style='position: relative; top: 50%%; left: 50%%; transform: translate(-50%%, -50%%);'>" (w+1) (h+1)
        "<g id='layer1'>"
    ] @
    svg @ 
    [
        "</g>"
        "</svg>"
    ]
