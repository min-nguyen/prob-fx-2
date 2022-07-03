#!/bin/bash
echo $PATH
cabal run prob-fx2 $1
sudo python3 graph.py $1