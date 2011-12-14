#!/usr/bin/env zsh

PARAMS="1024 768 1024 -1.351 0.06 0.0005 mandelbrot.png"
ghc -O2 -rtsopts -threaded mandelbrot.hs && \
    eval ./mandelbrot $PARAMS
