#!/bin/sh
# Syntax: ./run.sh {day-number}
echo "Running Advent of Code 2020 Day #$1 solution..."
sbcl --noinform --load day$1.lisp --non-interactive
