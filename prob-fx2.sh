#!/bin/bash
cabal run prob-fx2 $1
python3 graph.py  $1
