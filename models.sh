#!/bin/bash
echo $PATH
cabal run models $1
sudo python3 graph.py $1