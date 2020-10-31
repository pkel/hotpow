#!/bin/bash

# generate graphviz from block list
# ./this.sh output/blocks-34a005a8-27.csv | dot -Tsvg > chain.svg
# show using firefox

if [ -f "$1" ] ; then
  in="$1"
else
  echo provide block file as first argument
fi

echo "digraph \"$in\" {"

awk -F "\"*,\"*" 'NR>1 {print "n" $1 " [label=\"" $3 "\"] ; n" $1 " -> n" $2 " ;"}' "$in"

echo "}"
