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

`tests/Program.fs` is an entry point that writes a frame to `output.svg`, which is
the easiest way to look at what this produces without a browser.

To run it in a browser you would need the Fable toolchain
(`dotnet tool restore` with a manifest pinning `fable`, then `dotnet fable`) and a
bundler; neither is configured here any more.
