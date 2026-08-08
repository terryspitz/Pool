# F# / Fable SVG implementation

A separate, self-contained implementation of the caustics that renders to **SVG**
rather than to a canvas: it maps the refracted pool floor back to screen
coordinates one floor tile at a time, clipping each tile against the unit square.

This is where the F# earns its keep. `Point` and `Poly` in `Pool.fs` use operator
overloading, structural equality and exhaustive matching for the polygon
clipping, which is markedly more compact than the equivalent JavaScript would be.
The 9 tests under `tests/` cover that clipping.

## Status

**Frozen, and not part of the main page.** The page in `public/` is plain
JavaScript and does not reference anything here. This directory is not built in
CI and is not kept in sync with the JavaScript renderers — if the wave
parameters change over there, they will not change here.

It is known to build and pass its tests as of the last commit that touched it.

## Building

Needs the [.NET SDK](https://dotnet.microsoft.com/download) 10.0 or newer.

    dotnet build Pool.sln
    dotnet test Pool.sln

## One frame, as a file

`tests/Program.fs` is an entry point that writes a frame to `output.svg` in this
directory:

    dotnet run --project tests/PoolTest.fsproj

`poolHtml` returns a complete standalone `<svg>` document, so that file opens
directly in a browser over `file://`. Most of the run time is `Program.fs`
formatting the whole thing with `printfn "%A"`; generating the frame is about
20 ms.

## Animated, in a browser

`App.fs` fills the element with id `pool` and drives `requestAnimationFrame`.
Compiling it needs the Fable toolchain, pinned in `dotnet-tools.json`:

    dotnet tool restore
    dotnet fable App.fsproj --outDir build
    npx serve .                            # then open /index.html

Fable emits plain ES modules with relative imports, so no bundler is involved.
`index.html` is the host page; `build/` is generated and git-ignored. Serving
over http is required, as ES modules will not load over `file://`.

One thing to expect: `System.Random`'s seed is ignored under Fable, so the
browser picks a different wave field on every load, while the `output.svg` route
is reproducible. The two will not match each other.
