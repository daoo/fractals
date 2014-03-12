#!/usr/bin/env bash

width=$1
height=$2

ascii=./dist/build/fractals-ascii/fractals-ascii

echo -n
echo "<html><body><pre>"
$ascii mandelbrot 2 200 $width $height 4.3 2.41875 -2.15 1.209375 | fold -w $width
echo
echo "</pre></body></html>"
