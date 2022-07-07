#!/bin/bash
cabal run prob-fx2 $1
sudo python3 graph.py 
