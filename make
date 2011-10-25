#!/usr/bin/env zsh

ghc -O2 -rtsopts -threaded mandelbrot.hs && { \rm -f mandelbrot.png; ./mandelbrot && open mandelbrot.png }
