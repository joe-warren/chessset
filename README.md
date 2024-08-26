# Haskell Chess Set

Procedural chess set generator; the sets are designed to be 3D printable (although this is a WIP).

Implemented using [Waterfall-CAD](https://github.com/joe-warren/opencascade-hs/) ([hackage link](https://hackage.haskell.org/package/waterfall-cad)).

## Building 

Should support either the `stack` or `cabal` haskell build tools. 

You'll need the OpenCascade libraries installed, there are instructions for this in the [Waterfall-CAD repo](https://github.com/joe-warren/opencascade-hs/?tab=readme-ov-file#installing-dependencies).

Running the project will create a bunch of directories containing the generated chess sets. 

## Hacking

The entrypoint, and the top level descriptions of the individual chesssets, is in [`src/Sets.hs`](src/Sets.hs). 

That's probably a good starting point if you're looking to make modifications.

