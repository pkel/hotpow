#!/bin/bash

set -e

n_blocks=500
n_nodes=1000
n_runs=100

exe=_build/default/sim.exe

# Build static executable in container on machine w/o ocaml.
if [ "1" = "$STATIC" ] || ! command -v opam > /dev/null ; then
  exe=_build/static/bin/hotpow_sim
  if command -v podman > /dev/null ; then
    make docker_build_static 1>&2
  else
    make docker='sudo docker' docker_build_static 1>&2
  fi
fi

fair_alpha=$(bc -l <<< "1/$n_nodes")

# setup the parameter combinations
params () {
echo tag strategy n-blocks n-nodes quorum-size \
  alpha leader-failure-rate churn delta-block delta-vote

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

# propagation delay
parallel -j 1 echo delta \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" 0 0 "{2}" "{2}" \
  ::: 2 8 32 128 \
  ::: 0.1 0.0316 0.01 0.00316 0.001 0.000316 0.0001

# attacker
parallel -j 1 echo strategy \
  "{3}" $n_blocks $n_nodes "{1}" "{2}" 0 0 0 0 \
  ::: 1 2 4 8 16 32 64 128 256 \
  ::: 0.02 0.1 0.2 0.333333 0.5 \
  ::: naive censor

# realistic delta: vote 100ms, block 10s
parallel -j 1 echo delta_real \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" 0 0 0.01667 0.0001667 \
  ::: 2 8 32 128

# leader failure + realistic 100ms/10s delta
parallel -j 1 echo failure_real \
  naive $n_blocks $n_nodes "{1}" "$fair_alpha" "{2}" 0 0.01667 0.0001667 \
  ::: 2 8 32 128 \
  ::: 0 0.01 0.05 0.1 0.2 0.3 0.4 0.5

# attacker + realistic delta
parallel -j 1 echo strategy_real \
  "{3}" $n_blocks $n_nodes "{1}" "{2}" 0 0 0.01667 0.0001667 \
  ::: 1 2 4 8 16 32 64 128 256 \
  ::: 0.02 0.1 0.2 0.333333 0.5 \
  ::: naive censor
}

commit=$(git rev-parse --short HEAD || true)
commit=${commit-nogit}
outfile=sim_$commit.csv
logfile=sim_$commit.log
scrfile=sim_$commit.sh

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
    --delta-vote "{delta-vote}" \
    --delta-block "{delta-block}" \
    --alpha "{alpha}" \
    --strategy "{strategy}" \
    :::: <( echo seq ; seq 1 $n_runs ) \
    :::: <( params ) \
    | sed -u 's/\t//g'
  }

cp "$0" "$scrfile"
sim "$@" | tee "$outfile"
