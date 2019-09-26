#!/bin/bash

set -e

n_blocks=100
n_nodes=100
n_runs=1000

exe=_build/default/sim.exe

# Build static executable in container on machine w/o ocaml.
if [ "1" = "$STATIC" ] || ! command -v opam > /dev/null ; then
  exe=_build/static/sim.exe
  if command -v podman > /dev/null ; then
    make $exe 1>&2
  else
    make docker='sudo docker' $exe 1>&2
  fi
fi

fair_alpha=$(bc -l <<< "1/$n_nodes")

# setup the parameter combinations
params () {
echo tag strategy n-blocks n-nodes quorum-size \
  alpha leader-failure-rate churn latency vote-latency

# leader failure
parallel -j 1 echo failure \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" "{2}" 0 0 0 \
  ::: 2 8 32 128 \
  ::: 0 0.01 0.05 0.1 0.2 0.3 0.4 0.5

# churn
parallel -j 1 echo churn \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" 0 "{2}" 0 0 \
  ::: 2 8 32 128 \
  ::: 0 0.01 0.05 0.1 0.2 0.3 0.4 0.5

# latency
parallel -j 1 echo latency \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" 0 0 "{2}" "{2}" \
  ::: 2 8 32 128 \
  ::: 0.1 0.0316 0.01 0.00316 0.001 0.000316 0.0001

# attacker / strategy
parallel -j 1 echo strategy \
  "{3}" $n_blocks $n_nodes "{1}" "{2}" 0 0 0 0 \
  ::: 1 2 4 8 16 32 64 128 256 \
  ::: 0.02 0.1 0.2 0.333333 0.5 \
  ::: naive censor

# special case latency: vote 100ms, block 10s
parallel -j 1 echo latency_special \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" 0 0 0.01667 0.0001667 \
  ::: 2 8 32 128

# leader failure + 100ms/10s latency
parallel -j 1 echo failure_special \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" "{2}" 0 0.01667 0.0001667 \
  ::: 2 8 32 128 \
  ::: 0 0.01 0.05 0.1 0.2 0.3 0.4 0.5
}

commit=$(git rev-parse --short HEAD || true)
commit=${commit-nogit}
outfile=sim_$commit.csv
logfile=sim_$commit.log

sim () {
  printf "tag,run,"
  $exe --header

  parallel --header : --colsep " " \
    "$@" \
    --basefile $exe \
    --joblog "$logfile" \
    --resume-failed \
    --tagstring "{tag},{seq}," \
    $exe \
    --n-blocks "{n-blocks}" \
    --n-nodes "{n-nodes}" \
    --churn "{churn}" \
    --quorum-size "{quorum-size}" \
    --leader-failure-rate "{leader-failure-rate}" \
    --latency "{latency}" \
    --vote-latency "{vote-latency}" \
    --alpha "{alpha}" \
    --strategy "{strategy}" \
    :::: <( echo seq ; seq 1 $n_runs ) \
    :::: <( params ) \
    | sed -u 's/\t//g'
  }

sim "$@" | tee "$outfile"
