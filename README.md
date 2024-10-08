# Haskell Chess Set

Procedural chess set generator; the sets are designed to be 3D printable.

This project is written up [on my Blog](https://www.doscienceto.it/blog/posts/2024-09-15-chess-set.html).

STL files have been uploaded to [Thingiverse](https://www.thingiverse.com/thing:6767884) and [Printables](https://www.printables.com/model/1008592).

Implemented using [Waterfall-CAD](https://github.com/joe-warren/opencascade-hs/) ([hackage link](https://hackage.haskell.org/package/waterfall-cad)).

Units are in `cm`; this is a poor choice, and you're almost certainly going to need to scale the generated stl files by a factor of 10 when slicing them.

## Building 

Should support either the `stack` or `cabal` haskell build tools. 

You'll need the OpenCascade libraries installed, there are instructions for this in the [Waterfall-CAD repo](https://github.com/joe-warren/opencascade-hs/?tab=readme-ov-file#installing-dependencies).

Running the project will create a directory called `output` containing the generated chess sets. 

### Docker

I've also added a Docker image. 

This can be used if for you don't want to install a Haskell environment onto your base system. 

Personally, I can't understand why anyone would not want to install a Haskell dev environment, and drink deep from the font of monadic wisdom, but if you don't, you should be able to run the following to build the project, and also copy the generated `stls` out of it.


``` bash
docker build -t waterfall/haskell-chess-set .
containerID=$(docker create waterfall/haskell-chess-set)
docker cp ${containerID}:/opt/build/output ./docker-output
docker rm -v $containerID
```

## Hacking

The entrypoint, and the top level descriptions of the individual chesssets, is in [`src/Sets.hs`](src/Sets.hs). 

That's probably a good starting point if you're looking to make modifications.

## Images 

![](images/photo-black.jpg)
![](images/photo-white.jpg)
![](images/round.png)
![](images/square.png)
![](images/variable.png)
![](images/notation.png)
![](images/short-king.png)
