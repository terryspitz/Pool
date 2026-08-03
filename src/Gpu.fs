module Gpu

/// WebGL2 caustic renderer.
///
/// The 2D-canvas renderer walks a grid on the CPU, refracts each cell's four
/// corners and fills the resulting quad with an alpha inversely proportional to
/// its area. That is exactly a mesh draw, so here the same grid is handed to the
/// GPU as one indexed triangle draw and the vertex shader does the refraction.
///
/// Two things change in the process:
///  * the patch area is computed analytically. The displacement d(x,y) is a sum
///    of cosines, so its Jacobian is a sum of sines of the same arguments and
///    costs nothing extra; the area ratio of a refracted patch is |det(I + J)|,
///    the continuum limit of the cross-product area the CPU version measures.
///  * alpha is therefore per vertex and interpolates across each cell, instead
///    of being flat per quad. This removes the quantisation into 101 colour
///    steps and the faint seams between neighbouring quads.
///
/// WebGL is reached through dynamic interop so the project keeps its existing
/// package references - no WebGL binding package is needed.

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types

[<Emit("new Float32Array($0)")>]
let private newFloat32Array (len: int) : obj = jsNative

[<Emit("new Uint32Array($0)")>]
let private newUint32Array (len: int) : obj = jsNative

[<Emit("$0[$1] = $2")>]
let private setElem (arr: obj) (i: int) (v: float) : unit = jsNative

/// read a GL enum off the context rather than hard-coding its numeric value
[<Emit("$0[$1]")>]
let private glEnum (gl: obj) (name: string) : float = jsNative

/// texImage2D has more arguments than dynamic interop handles comfortably
[<Emit("$0.texImage2D($0.TEXTURE_2D, 0, $0.RGBA32F, $1, 1, 0, $0.RGBA, $0.FLOAT, $2)")>]
let private uploadWaveTexture (gl: obj) (width: int) (data: obj) : unit = jsNative

let vertexShaderSource = """#version 300 es
precision highp float;

// (i, j, amplitude, phase) per wave, one texel each
uniform sampler2D uWaves;
uniform int uNumWaves;
// speed*time already reduced mod 2 on the CPU: (|i|+|j|) is an integer and the
// phase is periodic in 2, so this is exact and keeps the argument small enough
// for a float to stay accurate however long the animation runs
uniform float uDrift;
uniform float uScaling;
uniform float uBrightness;
uniform float uAlphaGain;
// grid: vertex (0,0) sits at uOrigin, spacing uDs, uGridW vertices per row
uniform vec2 uOrigin;
uniform float uDs;
uniform int uGridW;
// world space -> clip space
uniform vec2 uViewScale;
uniform vec2 uViewOffset;

out float vAlpha;

const float PI = 3.141592653589793;

void main() {
    int gx = gl_VertexID % uGridW;
    int gy = gl_VertexID / uGridW;
    float x = uOrigin.x + float(gx) * uDs;
    float y = uOrigin.y + float(gy) * uDs;

    vec2 d = vec2(0.0);
    // Jacobian of d. d(alpha)/dx and d(beta)/dy share the same sine sum, so the
    // matrix is symmetric and only three entries are needed.
    float jxx = 0.0;
    float jxy = 0.0;
    float jyy = 0.0;
    for (int k = 0; k < uNumWaves; ++k) {
        vec4 wv = texelFetch(uWaves, ivec2(k, 0), 0);
        float fi = wv.x;
        float fj = wv.y;
        float amp = wv.z;
        // sgn(i)*i == abs(i), so the drift term depends only on |i|,|j|
        float theta = PI * (fi * x + fj * y + (abs(fi) + abs(fj)) * uDrift + wv.w);
        float c = cos(theta);
        float s = -PI * amp * sin(theta);
        d += amp * c * vec2(fi, fj);
        jxx += s * fi * fi;
        jxy += s * fi * fj;
        jyy += s * fj * fj;
    }
    d *= uScaling;
    jxx *= uScaling;
    jxy *= uScaling;
    jyy *= uScaling;

    // area of the refracted patch relative to the incoming patch
    float det = abs((1.0 + jxx) * (1.0 + jyy) - jxy * jxy);
    vAlpha = min(uBrightness / max(det, 1e-6), 1.0) * uAlphaGain;

    vec2 p = vec2(x, y) + d;
    gl_Position = vec4(p * uViewScale + uViewOffset, 0.0, 1.0);
}
"""

let fragmentShaderSource = """#version 300 es
precision mediump float;

uniform vec3 uColour;
in float vAlpha;
out vec4 fragColour;

void main() {
    fragColour = vec4(uColour, clamp(vAlpha, 0.0, 1.0));
}
"""

/// pool water colour, from pool.jpg (#146897)
let backgroundRgb = (0x14, 0x68, 0x97)
/// caustic colour, matching colours1 in Pool.fs
let causticRgb = (155, 255, 255)
/// colours1.[a] has alpha a/80 while a indexes alpha*100, i.e. a 100/80 gain
let alphaGain = 100. / 80.

type Renderer(canvas: HTMLCanvasElement) =
    let gl : obj = canvas?getContext("webgl2")
    do if isNull gl then failwith "WebGL2 is not available on this canvas"

    let ELEMENT_ARRAY_BUFFER = glEnum gl "ELEMENT_ARRAY_BUFFER"
    let STATIC_DRAW = glEnum gl "STATIC_DRAW"
    let TRIANGLES = glEnum gl "TRIANGLES"
    let UNSIGNED_INT = glEnum gl "UNSIGNED_INT"
    let COLOR_BUFFER_BIT = glEnum gl "COLOR_BUFFER_BIT"
    let BLEND = glEnum gl "BLEND"
    let SRC_ALPHA = glEnum gl "SRC_ALPHA"
    let ONE_MINUS_SRC_ALPHA = glEnum gl "ONE_MINUS_SRC_ALPHA"
    let VERTEX_SHADER = glEnum gl "VERTEX_SHADER"
    let FRAGMENT_SHADER = glEnum gl "FRAGMENT_SHADER"
    let COMPILE_STATUS = glEnum gl "COMPILE_STATUS"
    let LINK_STATUS = glEnum gl "LINK_STATUS"
    let TEXTURE_2D = glEnum gl "TEXTURE_2D"
    let TEXTURE0 = glEnum gl "TEXTURE0"
    let NEAREST = glEnum gl "NEAREST"
    let TEXTURE_MIN_FILTER = glEnum gl "TEXTURE_MIN_FILTER"
    let TEXTURE_MAG_FILTER = glEnum gl "TEXTURE_MAG_FILTER"
    let TEXTURE_WRAP_S = glEnum gl "TEXTURE_WRAP_S"
    let TEXTURE_WRAP_T = glEnum gl "TEXTURE_WRAP_T"
    let CLAMP_TO_EDGE = glEnum gl "CLAMP_TO_EDGE"

    let compile kind src =
        let shader = gl?createShader(kind)
        gl?shaderSource(shader, src)
        gl?compileShader(shader)
        let ok : bool = gl?getShaderParameter(shader, COMPILE_STATUS)
        if not ok then
            let log : string = gl?getShaderInfoLog(shader)
            failwithf "caustic shader failed to compile: %s" log
        shader

    let program =
        let p = gl?createProgram()
        gl?attachShader(p, compile VERTEX_SHADER vertexShaderSource)
        gl?attachShader(p, compile FRAGMENT_SHADER fragmentShaderSource)
        gl?linkProgram(p)
        let ok : bool = gl?getProgramParameter(p, LINK_STATUS)
        if not ok then
            let log : string = gl?getProgramInfoLog(p)
            failwithf "caustic program failed to link: %s" log
        p

    let uniform (name: string) : obj = gl?getUniformLocation(program, name)
    let uWaves = uniform "uWaves"
    let uNumWaves = uniform "uNumWaves"
    let uDrift = uniform "uDrift"
    let uScaling = uniform "uScaling"
    let uBrightness = uniform "uBrightness"
    let uAlphaGain = uniform "uAlphaGain"
    let uOrigin = uniform "uOrigin"
    let uDs = uniform "uDs"
    let uGridW = uniform "uGridW"
    let uViewScale = uniform "uViewScale"
    let uViewOffset = uniform "uViewOffset"
    let uColour = uniform "uColour"

    // the whole wave spectrum as an Nx1 RGBA32F texture, uploaded once
    let waveTexture = gl?createTexture()
    let numWaves =
        let f = Pool.frequencies
        let data = newFloat32Array (Pool.waveCount * 4)
        let mutable n = 0
        for i in -f..f do
            for j in -f..f do
                let k = Pool.waveIndex i j
                let amp = Pool.waveAmp.[k]
                if amp <> 0.0 then
                    setElem data (n*4)   (float i)
                    setElem data (n*4+1) (float j)
                    setElem data (n*4+2) amp
                    setElem data (n*4+3) Pool.wavePhase.[k]
                    n <- n + 1
        gl?bindTexture(TEXTURE_2D, waveTexture)
        uploadWaveTexture gl n data
        gl?texParameteri(TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST)
        gl?texParameteri(TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST)
        gl?texParameteri(TEXTURE_2D, TEXTURE_WRAP_S, CLAMP_TO_EDGE)
        gl?texParameteri(TEXTURE_2D, TEXTURE_WRAP_T, CLAMP_TO_EDGE)
        n

    let vao = gl?createVertexArray()
    let indexBuffer = gl?createBuffer()
    // vertex positions come from gl_VertexID, so the index buffer is the only
    // geometry the GPU needs and it only changes when the canvas is resized
    let mutable gridCols = 0
    let mutable gridRows = 0
    let mutable indexCount = 0

    let buildIndices cols rows =
        let quadsX = cols - 1
        let quadsY = rows - 1
        let count = quadsX * quadsY * 6
        let indices = newUint32Array count
        let mutable n = 0
        for qy in 0..quadsY-1 do
            for qx in 0..quadsX-1 do
                let v00 = qy*cols + qx
                let v10 = v00 + 1
                let v01 = v00 + cols
                let v11 = v01 + 1
                setElem indices n     (float v00)
                setElem indices (n+1) (float v10)
                setElem indices (n+2) (float v11)
                setElem indices (n+3) (float v00)
                setElem indices (n+4) (float v11)
                setElem indices (n+5) (float v01)
                n <- n + 6
        gl?bindVertexArray(vao)
        gl?bindBuffer(ELEMENT_ARRAY_BUFFER, indexBuffer)
        gl?bufferData(ELEMENT_ARRAY_BUFFER, indices, STATIC_DRAW)
        gridCols <- cols
        gridRows <- rows
        indexCount <- count

    do
        gl?enable(BLEND)
        gl?blendFunc(SRC_ALPHA, ONE_MINUS_SRC_ALPHA)

    /// vertices in the current mesh, for reporting
    member this.VertexCount = gridCols * gridRows
    member this.WaveCount = numWaves

    /// Draws one frame at the given wave time. `res` is pixels per grid cell,
    /// the same meaning as in the 2D renderer, but the GPU can afford it much
    /// finer.
    member this.Draw(time: float, res: float) =
        let cw = canvas.width
        let ch = canvas.height
        let w = int (cw / res)
        let h = int (ch / res)
        let minDim = min cw ch
        let ds = res / minDim
        let margin = Pool.marginCells ds
        let cols = w + margin*2
        let rows = h + margin*2
        if cols <> gridCols || rows <> gridRows then
            buildIndices cols rows
        let origin = -(float margin) * ds
        let r, g, b = backgroundRgb
        let cr, cg, cb = causticRgb

        gl?viewport(0, 0, cw, ch)
        gl?clearColor(float r / 255., float g / 255., float b / 255., 1.)
        gl?clear(COLOR_BUFFER_BIT)

        gl?useProgram(program)
        gl?bindVertexArray(vao)
        gl?activeTexture(TEXTURE0)
        gl?bindTexture(TEXTURE_2D, waveTexture)
        gl?uniform1i(uWaves, 0)
        gl?uniform1i(uNumWaves, numWaves)
        let drift = Pool.waveSpeed * time
        gl?uniform1f(uDrift, drift - 2. * floor (drift / 2.))
        gl?uniform1f(uScaling, Pool.waveScaling)
        gl?uniform1f(uBrightness, Pool.causticBrightness)
        gl?uniform1f(uAlphaGain, alphaGain)
        gl?uniform2f(uOrigin, origin, origin)
        gl?uniform1f(uDs, ds)
        gl?uniform1i(uGridW, cols)
        // world x spans [0, cw/minDim] and y spans [0, ch/minDim], y downwards
        gl?uniform2f(uViewScale, 2. * minDim / cw, -2. * minDim / ch)
        gl?uniform2f(uViewOffset, -1., 1.)
        gl?uniform3f(uColour, float cr / 255., float cg / 255., float cb / 255.)

        gl?drawElements(TRIANGLES, indexCount, UNSIGNED_INT, 0)

/// Returns a renderer, or None if this browser/canvas cannot provide WebGL2.
let tryCreate (canvas: HTMLCanvasElement) =
    try
        Some(Renderer(canvas))
    with e ->
        Browser.Dom.console.warn ("WebGL caustics unavailable, falling back to 2D canvas: " + e.Message)
        None
