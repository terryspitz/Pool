# Pool animation using caustics

Simulates a swimming pool with [waves](https://en.wikipedia.org/wiki/Wave_equation),
[refraction](https://en.wikipedia.org/wiki/Refraction) and
[caustics](https://en.wikipedia.org/wiki/Caustic_(optics)).

For each grid square representing an incoming patch of light, refract the four
corner coordinates onto the bottom surface of the pool and draw that
quadrilateral with an alpha inversely proportional to area, representing
dispersion of the energy.

[Try it now.](https://terryspitz.github.io/Pool/public/index.html)
[![caustic](png/Screenshot.png)](https://terryspitz.github.io/Pool/public/index.html)

The `?` button on the live page opens a short tutorial that steps through how the image is built —
undistorted grid, refraction, quad area as brightness — without ever pausing the animation
underneath, so the last step is just the tutorial getting out of the way.

## Running it

Plain JavaScript ES modules, with **no build step** — the browser loads the
source directly. Any static file server over the repository root works:

    npx serve .            # then open /public/index.html

Opening `public/index.html` from disk will not work: browsers refuse to load ES
modules over `file://`.

| URL | renderer |
| --- | --- |
| (default) | WebGL2 shader, falling back to the 2D canvas if unavailable |
| `?mode=canvas` | 2D canvas |
| `?res=4` | pixels per grid cell; smaller is finer and slower |

Click anywhere to stop and start the animation. Frames per second, milliseconds
per frame and the active renderer are shown in the corner.

## Layout

    src/waves.js    the wave field and the refraction offsets it produces
    src/canvas.js   2D canvas renderer
    src/gpu.js      WebGL2 renderer
    src/tutorial.js the in-page "how it's made" walkthrough
    src/app.js      renderer selection, animation loop, stats
    public/         the page
    bench/          accuracy and performance checks
    fsharp/         a separate, frozen F#/Fable implementation of the SVG
                    renderer; not used by the page, not built in CI

## How it works

### The wave field

The surface is a sum of `(2f+1)^2 = 361` cosines with random amplitudes and
phases, amplitude falling off as `1/(i^2 + j^2)`. What each renderer needs is the
refraction offset, the gradient of that surface, at every grid point.

### Separable evaluation

Evaluating all 361 cosines per grid point dominates the frame. The phase splits
into an x-only and a `(y,t)`-only part:

    theta = (x + sgn(i)*speed*t)*i + (y + sgn(j)*speed*t)*j + phase
          = i*x + (j*y + phase + (|i|+|j|)*speed*t)      since sgn(i)*i = |i|

so `cos(theta)` expands by the angle-addition rule and the whole j-sum hoists out
of the column loop, evaluated once per row. `cos(pi*i*x)` then depends only on
the grid, so it is tabulated when the canvas is resized rather than per frame.
Per row the cost drops from `columns * 361` transcendentals to `361` for the row
plus `columns * 9` multiply-adds.

`npm run bench` checks the rearrangement against the naive form — both using the
same cosine, so it isolates the loop structure — and times them:

    grid 192x108 (res=10), 244 columns/row, 361 waves, margin 26 cells
      separable vs naive: max abs diff 1.9e-15 of peak 1.9e-1
      agree to float precision

      naive      507 ms
      separable    5 ms   (95x faster)

### Grid margin

The grid extends past the screen so that refracted quads from off screen still
cover the edges. The offset is a sum of many cosines with independent phases, so
it is close to Gaussian; the margin is four standard deviations of it, in world
units, converted to cells. A fixed cell count would be either wasteful at coarse
resolutions or too small at fine ones.

### The two renderers

The **canvas** renderer walks the grid on the CPU, refracts each cell's four
corners, measures the resulting quadrilateral's area with a cross product and
fills it with an alpha from that area. Alpha is quantised into 101 steps, one
`fill()` per visible quad.

The **WebGL2** renderer hands the same grid to the GPU as one indexed triangle
draw. Vertex positions come from `gl_VertexID`, so the index buffer is the only
geometry uploaded and it only changes on resize; the 360 waves are an Nx1
`RGBA32F` texture of `(i, j, amplitude, phase)`. The patch area is analytic — the
offset is a sum of cosines, so its Jacobian is a sum of sines of the same
arguments and costs nothing extra, and the area ratio is `|det(I + J)|`, the
continuum limit of the cross-product area the canvas version measures. Alpha is
therefore per vertex and interpolates across each cell, which drops both the
quantisation into 101 steps and the faint seams between neighbouring quads.

`speed*time` is reduced mod 2 before being sent to the shader. `|i|+|j|` is an
integer and the phase has period 2, so this is exact, and it keeps the argument
small enough that a float stays accurate however long the animation runs.

`npm run bench-web` loads the real page in headless Chromium, checks that each
renderer initialises and draws, and compares them against each other on the same
grid at the same instant:

    res=10  gpu mean 174.5  canvas mean 169.8  (gpu +2.8%)
    res= 4  gpu mean 171.1  canvas mean 166.6  (gpu +2.7%)

They are close but not identical, and shouldn't be: flooring alpha into 101
buckets biases the canvas slightly dark. The check fails if they drift more than
6% apart.

Note that the container running these has no GPU, so WebGL there goes through the
SwiftShader software rasteriser — the timings from it are a correctness check,
not a representative frame rate.

### Ideas not taken

Batching the quads into one path per colour bucket, to cut ~20000 `fill()` calls
down to 101, is **much** slower (roughly 25x): Skia pays more to resolve one path
with thousands of subpaths than for the same number of small independent fills.

If more speed is wanted, the separable trick applies on the GPU too, as a
two-pass render — one pass into a float texture of row coefficients, then 9
iterations per vertex instead of 360.

## Benchmarks

    npm install
    npm run bench        # separable evaluation: accuracy and speed, in node
    npm run bench-web    # both renderers, in headless Chromium
