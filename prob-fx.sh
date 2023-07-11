#!/bin/bash
function exists_in_list() {
    LIST=$1
    DELIMITER=$2
    VALUE=$3
    [[ "$LIST" =~ ($DELIMITER|^)$VALUE($DELIMITER|$) ]]
}

possible_arguments="simLinRegr, lwLinRegr, ssmhLinRegr, smcLinRegr, rmpfLinRegr, pmhLinRegr, simSIR, simSIRS, simSIRSV, ssmhSIR, simLogRegr, lwLogRegr, ssmhLogRegr, simHMM, lwHMM, ssmhHMM, smcHMM, rmpfHMM, pmhHMM, simLDA, ssmhLDA, smcLDA, rmpfLDA, pmhLDA, simRadon, ssmhRadon, ssmhPredRadon, ssmhSchool, simGMM, ssmhGMM"

if [[ $# -eq 0 ]] || [ "$1" == "-h" ] || [ "$1" == "--help" ]; then
  echo "Usage: ./`basename $0` <arg>"
  # echo "Arg options: [$possible_arguments]"
elif exists_in_list "$possible_arguments" ", " $1; then
  cabal run prob-fx $1
  python3 graph.py $1
else
  echo "Argument '$1' unrecognized. "
  echo "Usage: ./`basename $0` <arg>"
  # echo "Arg options: [$possible_arguments]"
  exit
fi

