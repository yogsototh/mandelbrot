#!/usr/bin/env zsh

ghc -O2 -optl"-Wl,-no_compact_unwind,-no_pie" -rtsopts -threaded mandelbrot.hs && \
    ./mand
