#!/bin/bash
set -euo pipefail

# ------------------------------------------------------------------------------

# The opam switch that we want to use.
# This setting can be overridden on the command line.
#   e.g. switch=4.09.1 ./make.sh run stack

if [ -z ${switch+x} ] ; then
  switch=4.09.1+flambda ;
fi

# ------------------------------------------------------------------------------

# Compiling.

# Pass a relative directory as an argument, e.g. "src/iteration".
# Pass "." to compile all benchmarks.

make () {
  echo Compiling...
  (echo "(lang dune 2.0)" && \
   echo "(context (opam (switch $switch)))" \
  ) > dune-workspace.local
  echo Building using OCaml $switch...
  dune build --profile release --workspace dune-workspace.local "$1"
}

# ------------------------------------------------------------------------------

# Deal with the targets [all] and [switch] here, otherwise continue.

case "${1-all}" in
all)
  make "."
  exit 0
;;
switch)
  echo $switch
  exit 0
;;
esac

# ------------------------------------------------------------------------------

# Record the directory where this command is run, and restore it at the end.

home=$(pwd)
finish () {
  cd $home
}
trap finish EXIT

# ------------------------------------------------------------------------------

# Check if the variable $commit was defined on the command line.
# If so, create a temporary worktree with this commit, and move to it.

if ! [ -z ${commit+x} ] ; then
  # Normalize $commit to a short hash.
  commit=$(git rev-parse --short $commit)
  if [ -x /tmp/$commit ] ; then
    echo "Changing working directory to /tmp/$commit/..." ;
  else
    echo "Creating git worktree /tmp/$commit..." ;
    git worktree add /tmp/$commit $commit ;
  fi
  if [ -d /tmp/$commit/benchmark/previous ] ; then
    # This used to be the name of the main benchmark directory.
    cd /tmp/$commit/benchmark/previous
  else
    cd /tmp/$commit/benchmark
  fi
fi

# Then, regardless of whether $commit was defined, define $commit to
# be the short hash of the current commit.

commit=$(git rev-parse --short HEAD)

# Finally, if the command is "run", then stay in the current directory,
# which corresponds to $commit, otherwise move back to $home.

case "${1-all}" in
run)
;;
*)
  echo "Changing working directory back to $home..." ;
  cd $home
;;
esac

# ------------------------------------------------------------------------------

# Find our relative path with respect to the project root,
# and find the image of the current directory in the dune's
# _build directory.

relative () {
  if [ -f dune-project ] ; then
    image="${image}_build/${switch}"
  else
    image="../${image}"
    local here=$(basename $(pwd))
    cd ..
    relative
    cd ${here}
    image="${image}/${here}"
  fi
}

image=""
relative
echo "Our build directory is ${image}"

# ------------------------------------------------------------------------------

# The name of the executables built by dune.
# They are of the form $bin/$benchmark/Main.exe.

bin=${image}/src

# A naming convention for all files (basename).
# Use like this: `base $benchmark`

base () {
  echo $1.$commit.$switch
}

# A naming convention for textual result files.
# Use like this: `result $benchmark`

result () {
  echo `base $1`.out
}

# A naming convention for graphics files.
# Use like this: `graphics $benchmark`

graphics () {
  echo `base $1`.pdf
}

# A naming convention for comparisons.

# We assume that the results files that are being compared
# are listed in the file .comparison.
# We read their names (say, foo, bar, quux) and build a
# composite name of the form foo-versus-bar-versus-quux.

comparisonbase () {
  name=""
  for f in `cat .comparison` ; do
    f=$(basename $f) ;
    f=${f%.out} ;
    if [ -z $name ] ; then
      name=$f ;
    else
      name="$name-versus-$f"
    fi
  done
  echo $name
}

# ------------------------------------------------------------------------------

# Viewing the most-recently-generated PDF file.

# We assume that the name of this file has been written to .graphics.

view () {
  graphics=$(cat $home/.graphics)
  if [ -x /usr/bin/open ] ; then \
    open $graphics ; \
  elif command -v evince >/dev/null ; then \
    evince $graphics ; \
  fi
}

# ------------------------------------------------------------------------------

# Abbreviations for thousands, millions, etc. Use $K, $M, $G.

K=000
M=000000
G=000000000

# ------------------------------------------------------------------------------

# Fixing the prog field in a result file.

# We replace the full name of the executable in the "prog" field
# with $base.

# Use like this: fixprog $benchmark

fixprog () {
  benchmark="$1"
  result=$home/`result $benchmark`
  base=`base $benchmark`
  sed -e "s/prog.*\$/prog $base/" -i.bak $result && rm $result.bak
}

# ------------------------------------------------------------------------------

# A generic function for plotting a comparison.

# We assume that there exists a function named plot_${benchmark},
# which expects three arguments, $result, $graphics, $series.

# We assume that the benchmark results are stored in $comparisonbase.out.

plot_comparison () {
  comparisonbase=`comparisonbase`
  result=$comparisonbase.out
  graphics=$comparisonbase.pdf
  series=prog,
  benchmark=`cat .benchmark`
  plot_${benchmark} $result $graphics $series
}

# ------------------------------------------------------------------------------

# Auxiliary functions.

run_prologue () {
  # Record which benchmark was last run.
  echo $benchmark > $home/.benchmark
  # Construct the name of the executable.
  prog=$bin/$benchmark/Main.exe
  # Construct the full name of the results file.
  # (The benchmark may be run in a directory other than $home.)
  result=`result $benchmark`
  result=$home/$result
}

run_postlogue () {
  # Construct the short name of the results file.
  result=`result $benchmark`
  # Write this to .result and .comparison.
  echo $result > $home/.result
  echo $result >> $home/.comparison
  # Fix the name of the executable in the results file.
  fixprog $benchmark
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# The stack benchmark.

# Use like this:
# ./make.sh run stack    # or the following line:
# ./make.sh run bigstack # runs a more ambitious benchmark
# ./make.sh plot stack

run_stack () {
  benchmark=stack
  make src/$benchmark
  run_prologue
  stacks=ESek,PSek
  prun -prog $prog \
    -seq $stacks \
    -n 4$M \
    -length 2,10,100,1$K,10$K,100$K,1$M \
    -chunk_capacity 128 \
    -deep_chunk_capacity 16 \
    -runs 3 \
    -timeout 10 \
    -output $result
  run_postlogue
}

run_bigstack () {
  benchmark=stack
  make src/$benchmark
  run_prologue
  stacks=StackListRef,StackFixedArray,StackOCamlStdlib,StackVector,ESek,PSek
  prun -prog $prog \
    -n 40$M \
    -length 2,10,100,1$K,10$K,100$K,1$M,10$M,20$M,40$M \
    -seq $stacks \
    -chunk_capacity 128 \
    -deep_chunk_capacity 16 \
    -runs 3 \
    -timeout 10 \
    -output $result
  run_postlogue
}

plot_stack () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -series ${series}seq \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The outermost chunk capacity benchmark. (Based on the stack benchmark.)

run_capacity () {
  benchmark=capacity
  make src/stack
  run_prologue
  prog=$bin/stack/Main.exe
  stacks=ESek,PSek
  prun -prog $prog \
    -seq $stacks \
    -n 500$K \
    -length 10,1$K,100$K,500$K \
    -chunk_capacity 32,64,128,256 \
    -deep_chunk_capacity 32 \
    -runs 3 \
    -timeout 5 \
    -output $result
  run_postlogue
}

plot_capacity () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -chart seq \
    -series ${series}chunk_capacity \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The worst-case scenario benchmark. (Based on the stack benchmark.)

run_scenario () {
  benchmark=scenario
  make src/stack
  run_prologue
  prog=$bin/stack/Main.exe
  stacks=ESek
  prun -prog $prog \
    -seq $stacks \
    -n 5$M \
    -length 1,10,100,1$K,500$K \
    -nb_init 0,50,256,255,257 \
    -chunk_capacity 128 \
    -deep_chunk_capacity 32 \
    -runs 3 \
    -timeout 5 \
    -output $result
  run_postlogue
}

plot_scenario () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -chart seq \
    -series ${series}nb_init \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The functor benchmark. (Based on the stack benchmark.)

run_functor () {
  benchmark=functor
  make src/stack
  run_prologue
  prog=$bin/stack/Main.exe
  stacks=StackVector,StackVectorNoFunctor
  prun -prog $prog \
    -seq $stacks \
    -n 16777216 \
    -length 1,32,1024,131072,4194304,16777216 \
    -runs 3 \
    -timeout 5 \
    -output $result
  run_postlogue
}

plot_functor () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -series ${series}seq \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The overwrite benchmark. (Based on the stack benchmark.)

run_overwrite () {
  benchmark=overwrite
  make src/stack
  run_prologue
  prog=$bin/stack/Main.exe
  stacks=ESek,PSek
  prun -prog $prog \
    -seq $stacks \
    -n 10$M \
    -length 2,10,1$K,1$M,10$M \
    -overwrite_empty_slots 0,1 \
    -runs 3 \
    -timeout 5 \
    -output $result
  run_postlogue
}

plot_overwrite () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -chart seq \
    -series ${series}overwrite_empty_slots \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The heap benchmark. (Based on the stack benchmark.)

run_heap () {
  benchmark=heap
  make src/stack
  run_prologue
  prog=$bin/stack/Main.exe
  stacks=ESek,PSek
  prun -prog $prog \
    -seq $stacks \
    -n 10$M \
    -length 2,10,1$K,1$M,10$M \
    -minor_heap_multiple_of_32k 1,4,16,32,128,256 \
    -runs 3 \
    -timeout 5 \
    -output $result
  run_postlogue
}

plot_heap () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x length -y exectime \
    -legend-pos topleft \
    -chart seq \
    -series ${series}minor_heap_multiple_of_32k \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The flatten benchmark.

# seq can be ESek, PSek.

run_flatten () {
  benchmark=flatten
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -seq ESek,PSek \
    -n 320,1$K,3200,10$K,32$K,100$K,320$K,640$K,1$M,1600$K,3200$K,10$M,32$M \
    -m 100 \
    -output $result
  run_postlogue
}

plot_flatten () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}seq \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The construction benchmark.

# seq can be Array, ESek, PSek.

# op can be push, init, make.

# The combination Array/push is currently not supported.

run_construction () {
  benchmark=construction
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -seq Array,ESek,PSek \
    -op init,make \
    -n 100,320,1$K,3200,10$K,32$K,100$K,320$K,1$M,3200$K,10$M,32$M,100$M \
    -output $result
  run_postlogue
}

plot_construction () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}seq,op \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The iteration benchmark.

# The iteration method "get" is not benchmarked because it is too inefficient
# in comparison with the others.

# The construction methods include "push" and "assemble". The latter is slower.

# The supported sequence types include ESek and PSek.

# The iteration methods "sam" and "gsam" (set-and-move; get-set-and-move)
# are supported by ESek only.

run_iteration () {
  benchmark=iteration
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -n 1$K,10$K,100$K,1$M,2$M,4$M,8$M,16$M,32$M,64$M,128$M \
    -method iter,iters,gam,gasj,sam \
    -construction push \
    -seq ESek \
    -check_iterator_validity 0 \
    -output $result
  run_postlogue
}

plot_iteration () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}method \
    -chart seq \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The reach benchmark.

# The construction methods include "push" and "assemble". The latter is slower.

# TODO : decide how to display benchmarks at nearby jump distances,
#        such as 255, 256, 257.

run_reach () {
  benchmark=reach
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -n 10$K,20$K,40$K,80$K,160$K,320$K,640$K,1$M,2$M \
    -distance -1,8357 \
    -output $result
  run_postlogue
}

plot_reach () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}distance \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The split benchmark.

# The construction methods include "push", "assemble" (slower), "share".

# The operations include "get", "drop", "take", "split", "concat",
# "create", "create+reach", "reach".

# The parameter n must be provided.

run_split () {
  benchmark=split
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -n 1$K,3$K,10$K,30$K,70$K,100$K,200$K,300$K,1$M,3$M,9$M \
    -op get,drop,take,split,concat,reach \
    -seq PSek,ESek \
    -seed 52 \
    -output $result
  run_postlogue
}

plot_split () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}op \
    -chart seq \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The fill benchmark.

# The operations include fill and blit.

# The construction methods include "push", "assemble" (slower), "share".

# The parameter n must be provided.

run_fill () {
  benchmark=fill
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -n 1$K,10$K,100$K,1$M,2$M,4$M,8$M \
    -construction push,share \
    -op fill,blit \
    -output $result
  run_postlogue
}

plot_fill () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y cycles \
    -legend-pos topleft \
    -series ${series}construction,op \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The traversal benchmark.

# The parameter n must be provided.

# Picking a max out degree of 5 seems to allow about 90%
# of the vertices to be reached.

# n=50M requires about 4Gb RAM.

run_traversal () {
  benchmark=traversal
  make src/$benchmark
  run_prologue
  prun -prog $prog \
    -n 1$K,10$K,100$K,1$M `# 10$M,50$M` \
    -d 5 \
    -traversal bfs/ESek,bfs/Queue,dfs/ESek,dfs/Stack,dfs/StackFixedArray \
    -output $result
  run_postlogue
}

plot_traversal () {
  result="$1"
  graphics="$2"
  series="$3"
  echo $graphics > $home/.graphics
  pplot scatter \
    --yzero --xlog -x n -y allocated \
    -legend-pos topleft \
    -series ${series}traversal \
    -input $result \
    -output $graphics
}

# ------------------------------------------------------------------------------

# The main program.

# Find out which benchmark was run last.
# We use this as a default value in "./make.sh run" and "./make.sh plot",
# but allow it to be overridden by the user.
if [ -f .benchmark ] ; then
  benchmark=`cat .benchmark` ;
else
  benchmark=stack # (default value)
fi

case "${1-all}" in
list)
  echo "stack      # or: bigstack"
  echo "capacity   # outermost most chunk capacity in the stack benchmark"
  echo "scenario   # a worst-case scenario in the stack benchmark"
  echo "functor    # effect of a functor in the stack benchmark"
  echo "overwrite  # effect of overwriting empty slots in the stack benchmark"
  echo "heap       # effect of the minor heap size in the stack benchmark"
  echo "flatten"
  echo "construction"
  echo "iteration"
  echo "reach"
  echo "split"
  echo "fill"
  echo "traversal"
;;
run)
  benchmark=${2-${benchmark}}
  run_${benchmark}
;;
plot)
  benchmark=${2-${benchmark}}
  result=`result $benchmark`
  graphics=`graphics $benchmark`
  series=""
  plot_${benchmark} $result $graphics "$series"
;;
compare)
  # Here, we expect the current directory to be $home.
  case "${2-start}" in
  start)
    # Reset the list of results files that should be compared.
    rm -f .comparison
    touch .comparison
  ;;
  stop)
    # Choose a base name for the comparison file.
    comparisonbase=`comparisonbase`
    # Concatenate the results files.
    echo "Writing all results to $comparisonbase.out..."
    cat `cat .comparison` > $comparisonbase.out
  ;;
  plot)
    plot_comparison
  ;;
  esac
;;
view)
  view
;;
clean)
  dune clean
  rm -rf `cat .gitignore`
;;
realclean)
  ./make.sh clean
  rm -f *.out *.pdf
;;
gc)
  for w in `git worktree list --porcelain | grep worktree | grep /tmp | cut -d ' ' -f 2` ; do
    echo "Removing git worktree $w..." ;
    git worktree remove $w ;
  done
;;
*)
  echo "Unknown command: $1"
  echo "Do you mean: run <benchmark>, plot <benchmark>, view, clean, realclean?"
  exit 1
;;
esac
