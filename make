#!/usr/bin/env zsh

ghc -O2 -rtsopts -threaded mandelbrot.hs && \
    ./mand
