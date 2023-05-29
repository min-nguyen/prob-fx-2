#!/bin/bash
function exists_in_list() {
    LIST=$1
    DELIMITER=$2
    VALUE=$3
    [[ "$LIST" =~ ($DELIMITER|^)$VALUE($DELIMITER|$) ]]
}

possible_arguments="simLinRegrOnce, lwLinRegrOnce, mhLinRegrOnce, simLinRegr, lwLinRegr, mhLinRegr, smcLinRegr, rmsmcLinRegr, pmmhLinRegr, smc2LinRegr, bbviLinRegr, mleLinRegr, mapLinRegr, simSIR, simSIRS, simSIRSV, mhSIR, simLogRegrOnce, lwLogRegrOnce, mhLogRegrOnce, simLogRegr, lwLogRegr, mhLogRegr, simHMM, lwHMM, mhHMM, smcHMM, rmsmcHMM, pmmhHMM, smc2HMM, bbviHMM, mleHMM, mapHMM, inviHMM, simLDA, mhLDA, smcLDA, rmsmcLDA, pmmhLDA, bbviLDA, mleLDA, mapLDA, simRadon, mhRadon, mhPredRadon, mhSchool, simGMM, mhGMM"

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

