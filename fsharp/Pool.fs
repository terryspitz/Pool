module Pool

open System
open System.Diagnostics
open Fable.Core
open Browser
open Browser.Types

//TODO:
// apply caustics and projection together
// add specular highlights
// implement in shadertoy.com

let tau = 2.0 * Math.PI
let frequencies = 9
/// side length of the (i,j) frequency grid
let freqSide = frequencies*2+1
let waveCount = freqSide*freqSide
/// index into the wave tables for frequency pair (i,j)
let inline waveIndex i j = (i+frequencies)*freqSide + (j+frequencies)
/// Flat arrays rather than a 2D wrapper: the tuple-indexed accessor allocated a
/// tuple on every read, which in the inner loop was hundreds of allocations per pixel.
let waveAmp : float[] = Array.zeroCreate waveCount
let wavePhase : float[] = Array.zeroCreate waveCount
let waveSpeed = 0.002
let waveScaling = 0.1
/// alpha of a refracted patch is min (causticBrightness/area) 1.0
let causticBrightness = 0.6
/// how many standard deviations of refraction offset the grid margin must cover
let marginSigma = 4.0

let rnd = System.Random(42)  // seeds not supported (ignored) by Fable or js
for i in -frequencies..frequencies do
    for j in -frequencies..frequencies do
        if i<>0 || j<>0 then
            waveAmp.[waveIndex i j] <- (rnd.NextDouble() - 0.5) /  (float ((i*i + j*j)))
            wavePhase.[waveIndex i j] <- rnd.NextDouble() * tau

/// Grid margin needed so that refracted quads from off screen still reach the
/// screen edges, in world units. The offset is a sum of many cosines with
/// independent phases, so it is near Gaussian and marginSigma sigma covers it.
/// This used to be a fixed 5 cells, which is only about the right size at
/// res=30 and leaves uncovered wedges at the edges at finer resolutions.
let marginWorld =
    let mutable vx = 0.
    let mutable vy = 0.
    for i in -frequencies..frequencies do
        for j in -frequencies..frequencies do
            let a = waveAmp.[waveIndex i j]
            let ai = a * float i
            let aj = a * float j
            vx <- vx + ai*ai/2.
            vy <- vy + aj*aj/2.
    marginSigma * waveScaling * sqrt (max vx vy)

/// the margin above expressed in grid cells of size ds
let marginCells ds = max 5 (int (ceil (marginWorld / ds)))

/// approximate cos with two parabolas
/// from https://gist.github.com/geraldyeo/988116 then simplified
/// note: x is radians/PI, i.e. fastCos x ~= cos (PI*x)
/// Kept for reference/tests; the renderers now use the real cosine, which the
/// separable form below makes affordable (and which is ~5% more accurate).
let fastCos x =
    let mutable x = x + 1.5
    let xx = 2. * floor (x / 2.)
    x <- x - xx - 1.
    assert (-1.<=x && x<1.)
    if x < 0. then
        4. * (1.+x) * x
    else
        4. * (1.-x) * x

// ---------------------------------------------------------------------------
// Separable wave evaluation.
//
// The height field is  sum over i,j of  amp * cos(PI * theta)  with
//     theta = (x + sgn i * speed * t) * i + (y + sgn j * speed * t) * j + phase
// and since sgn(i)*i = |i| this is
//     theta = i*x  +  (j*y + phase + (|i|+|j|)*speed*t)
//           = i*x  +  psi(i,j,y,t)
// The second term does not depend on x, so
//     cos(PI*theta) = cos(PI*i*x)*cos(PI*psi) - sin(PI*i*x)*sin(PI*psi)
// which lets the whole j-sum be hoisted out of the column loop (done once per
// row) and the cos/sin of PI*i*x be tabulated once per grid rather than per row.
//
// Cost per row drops from  columns * (2f+1)^2  transcendentals to
// (2f+1)^2 for the row plus  columns * f  multiply-adds: ~45x fewer operations
// at f=9 (measured in bench/derivs-bench.js).
// ---------------------------------------------------------------------------

/// cos and sin of (PI * i * x) for i = 1..frequencies at every column of the
/// grid, where x = (col-margin)*ds. Depends only on the grid geometry, so it is
/// rebuilt when the canvas is resized rather than every frame.
type ColTable(cols: int, ds: float, margin: int) =
    let cosTab : float[] = Array.zeroCreate (cols*frequencies)
    let sinTab : float[] = Array.zeroCreate (cols*frequencies)
    do
        for col in 0..cols-1 do
            let a = Math.PI * float (col-margin) * ds
            let c1 = cos a
            let s1 = sin a
            // angle-multiple recurrence: (c,s) for i+1 from (c,s) for i
            let mutable c = c1
            let mutable s = s1
            for i in 0..frequencies-1 do
                cosTab.[col*frequencies+i] <- c
                sinTab.[col*frequencies+i] <- s
                let nc = c*c1 - s*s1
                s <- s*c1 + c*s1
                c <- nc
    member this.Cols = cols
    member this.Ds = ds
    member this.Margin = margin
    member this.Cos = cosTab
    member this.Sin = sinTab

let mutable cachedTable : ColTable = ColTable(1, 1., 0)

/// Returns a column table for this grid, reusing the cached one when unchanged.
let colTable cols ds margin =
    if cachedTable.Cols <> cols || cachedTable.Ds <> ds || cachedTable.Margin <> margin then
        cachedTable <- ColTable(cols, ds, margin)
    cachedTable

// Row coefficients: the j-sums, already folded over +/-i using
// cos(-a)=cos a and sin(-a)=-sin a. Indexed 1..frequencies.
let private rowA : float[] = Array.zeroCreate (frequencies+1)
let private rowB : float[] = Array.zeroCreate (frequencies+1)
let private rowC : float[] = Array.zeroCreate (frequencies+1)
let private rowD : float[] = Array.zeroCreate (frequencies+1)
/// the i=0 term, which contributes nothing to dx and a constant to dy
let mutable rowC0 = 0.0

let private calcRowCoeffs y time =
    let st = waveSpeed * time
    let mutable c0 = 0.0
    for j in -frequencies..frequencies do
        let k = waveIndex 0 j
        let psi = Math.PI * (float j * y + float (abs j) * st + wavePhase.[k])
        c0 <- c0 + waveAmp.[k] * float j * cos psi
    rowC0 <- c0
    for i in 1..frequencies do
        let mutable ap = 0.
        let mutable bp = 0.
        let mutable cp = 0.
        let mutable dp = 0.
        let mutable am = 0.
        let mutable bm = 0.
        let mutable cm = 0.
        let mutable dm = 0.
        for j in -frequencies..frequencies do
            let basePhase = float j * y + float (i + abs j) * st
            let kp = waveIndex i j
            let ampP = waveAmp.[kp]
            let psiP = Math.PI * (basePhase + wavePhase.[kp])
            let cP = cos psiP
            let sP = sin psiP
            ap <- ap + ampP * cP
            bp <- bp + ampP * sP
            cp <- cp + ampP * float j * cP
            dp <- dp + ampP * float j * sP
            let km = waveIndex (-i) j
            let ampM = waveAmp.[km]
            let psiM = Math.PI * (basePhase + wavePhase.[km])
            let cM = cos psiM
            let sM = sin psiM
            am <- am + ampM * cM
            bm <- bm + ampM * sM
            cm <- cm + ampM * float j * cM
            dm <- dm + ampM * float j * sM
        rowA.[i] <- float i * (ap - am)
        rowB.[i] <- float i * (bp + bm)
        rowC.[i] <- cp + cm
        rowD.[i] <- dp - dm

/// Fills dxs/dys with the (dx,dy) refraction offsets for every column at row height y.
/// The derivatives represent the amount of refraction horizontally,
/// i.e. the cos of angle of wave surface normal from vertical at each point in x and y direction.
let CalcDerivsRow (dxs: float[]) (dys: float[]) (tab: ColTable) y time =
    calcRowCoeffs y time
    let cosTab = tab.Cos
    let sinTab = tab.Sin
    for col in 0..dxs.Length-1 do
        let o = col*frequencies
        let mutable dx = 0.
        let mutable dy = rowC0
        for i in 1..frequencies do
            let c = cosTab.[o+i-1]
            let s = sinTab.[o+i-1]
            dx <- dx + c*rowA.[i] - s*rowB.[i]
            dy <- dy + c*rowC.[i] - s*rowD.[i]
        dxs.[col] <- dx * waveScaling
        dys.[col] <- dy * waveScaling

[<Struct>]
type Point = { X: float; Y: float }
with
    override this.ToString() = sprintf "%.4f,%.4f" this.X this.Y
    static member (+) (p1, p2) = {X=p1.X+p2.X; Y=p1.Y+p2.Y}
    static member (-) (p1, p2) = {X=p1.X-p2.X; Y=p1.Y-p2.Y}
    ///rotates by 90deg clockwise about (0.5,0.5)
    member this.Rotate90C = {X=this.Y; Y=1.0-this.X}
    member this.PosX = this.X > 0.0

type Poly = { pts: Point[] }
with
    static member (+) (poly,pt) = {pts=Array.map (fun p -> p+pt) poly.pts}
    static member (-) (poly,pt) = {pts=Array.map (fun p -> p-pt) poly.pts}
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
    member this.CropUnitSquareAt pt = ((this-pt).CropUnitSquare) + pt
    member this.ToSvg colour =
        if this.pts.Length>0 then
            [
                "<path d='"
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

/// Render causics as SVG
/// Map the distorted pool bottom back to screen coords in tiles
let poolHtml time =
    let mult = 2
    let w = 16 * mult
    let h = 9 * mult
    let scale = float (min w h)
    let ds = 1.0 / scale
    let grid = scale
    let tab = colTable (w+1) ds 0
    // these are copied rather than ping-ponged: the row loop below is a sequence
    // expression, which cannot capture a mutable binding
    let d1x : float[] = Array.zeroCreate (w+1)
    let d1y : float[] = Array.zeroCreate (w+1)
    let d2x : float[] = Array.zeroCreate (w+1)
    let d2y : float[] = Array.zeroCreate (w+1)
    CalcDerivsRow d1x d1y tab 0. time
    let svg =
        [for row in 0..h-1 do
            let rr = float row * ds
            CalcDerivsRow d2x d2y tab (rr+ds) time
            for col in 0..w-1 do
                let debug = row=0 && col=0
                let debug = false
                let cc = float col * ds
                let xtl = cc    + d1x.[col]
                let ytl = rr    + d1y.[col]
                let xtr = cc+ds + d1x.[col+1]
                let ytr = rr    + d1y.[col+1]
                let xbl = cc    + d2x.[col]
                let ybl = rr+ds + d2y.[col]
                let xbr = cc+ds + d2x.[col+1]
                let ybr = rr+ds + d2y.[col+1]
                if debug then
                    printfn "\n%d %d M %f , %f L %f , %f L %f , %f L %f , %f" row col xtl ytl xtr ytr xbr ybr xbl ybl
                let poly = {pts=[|{X=xtl*scale;Y=ytl*scale}; {X=xtr*scale;Y=ytr*scale};
                                  {X=xbr*scale;Y=ybr*scale}; {X=xbl*scale;Y=ybl*scale}|]}
                let simple = false
                if simple then
                    yield! poly.ToSvg (if ((col+5)%(w/4))<2 then "4444ff" else "none")
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
                        if i%5<2 || j%5<2 then
                            if debug then
                                printfn "a-e u,v= %f %f %f %f %f %f %f %f " a b c d e f u v
                                printfn "i,j= %d %d" i j
                            let tile = poly.CropUnitSquareAt {X=float i;Y=float j}
                            let projTile = {pts=[|for p in tile.pts do screen_coords (p.X/grid) (p.Y/grid)|]}
                            let s0 = {X=float col; Y=float row}
                            yield! (projTile+s0).ToSvg "4444ff"
            for col in 0..w do
                d1x.[col] <- d2x.[col]
                d1y.[col] <- d2y.[col]
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
