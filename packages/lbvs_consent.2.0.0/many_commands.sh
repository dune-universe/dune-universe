#!/bin/bash

#set -x

# # convert all files with MACSS FPs into files with ECFP4 FPs
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_alpha/+/queries.csv  > data/ER_alpha/+/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_alpha/-/queries.csv  > data/ER_alpha/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/RAR_alpha/-/queries.csv > data/RAR_alpha/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_beta/+/queries.csv   > data/ER_beta/+/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/GR/+/queries.csv        > data/GR/+/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/GR/-/queries.csv        > data/GR/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/AR/-/queries.csv        > data/AR/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/RXR_alpha/-/queries.csv > data/RXR_alpha/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/PR/+/queries.csv        > data/PR/+/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/PR/-/queries.csv        > data/PR/-/queries.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_alpha/+/db.csv  > ./data/ER_alpha/+/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_alpha/-/db.csv  > ./data/ER_alpha/-/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/RAR_alpha/-/db.csv > ./data/RAR_alpha/-/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/ER_beta/+/db.csv   > ./data/ER_beta/+/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/GR/+/db.csv        > ./data/GR/+/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/GR/-/db.csv        > ./data/GR/-/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/AR/-/db.csv        > ./data/AR/-/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/RXR_alpha/-/db.csv > ./data/RXR_alpha/-/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/PR/+/db.csv        > ./data/PR/+/db.ecfp4
# ./bin/csv2ecfp4.sh data/name2ecfp4.csv data/PR/-/db.csv        > ./data/PR/-/db.ecfp4

# wc -l data/ER_alpha/+/queries.csv  data/ER_alpha/+/queries.ecfp4
# wc -l data/ER_alpha/-/queries.csv  data/ER_alpha/-/queries.ecfp4
# wc -l data/RAR_alpha/-/queries.csv data/RAR_alpha/-/queries.ecfp4
# wc -l data/ER_beta/+/queries.csv   data/ER_beta/+/queries.ecfp4
# wc -l data/GR/+/queries.csv        data/GR/+/queries.ecfp4
# wc -l data/GR/-/queries.csv        data/GR/-/queries.ecfp4
# wc -l data/AR/-/queries.csv        data/AR/-/queries.ecfp4
# wc -l data/RXR_alpha/-/queries.csv data/RXR_alpha/-/queries.ecfp4
# wc -l data/PR/+/queries.csv        data/PR/+/queries.ecfp4
# wc -l data/PR/-/queries.csv        data/PR/-/queries.ecfp4

# wc -l data/ER_alpha/+/db.csv  ./data/ER_alpha/+/db.ecfp4
# wc -l data/ER_alpha/-/db.csv  ./data/ER_alpha/-/db.ecfp4
# wc -l data/RAR_alpha/-/db.csv ./data/RAR_alpha/-/db.ecfp4
# wc -l data/ER_beta/+/db.csv   ./data/ER_beta/+/db.ecfp4
# wc -l data/GR/+/db.csv        ./data/GR/+/db.ecfp4
# wc -l data/GR/-/db.csv        ./data/GR/-/db.ecfp4
# wc -l data/AR/-/db.csv        ./data/AR/-/db.ecfp4
# wc -l data/RXR_alpha/-/db.csv ./data/RXR_alpha/-/db.ecfp4
# wc -l data/PR/+/db.csv        ./data/PR/+/db.ecfp4
# wc -l data/PR/-/db.csv        ./data/PR/-/db.ecfp4

# head -1 data/ER_alpha/+/queries.csv  data/ER_alpha/+/queries.ecfp4
# head -1 data/ER_alpha/-/queries.csv  data/ER_alpha/-/queries.ecfp4
# head -1 data/RAR_alpha/-/queries.csv data/RAR_alpha/-/queries.ecfp4
# head -1 data/ER_beta/+/queries.csv   data/ER_beta/+/queries.ecfp4
# head -1 data/GR/+/queries.csv        data/GR/+/queries.ecfp4
# head -1 data/GR/-/queries.csv        data/GR/-/queries.ecfp4
# head -1 data/AR/-/queries.csv        data/AR/-/queries.ecfp4
# head -1 data/RXR_alpha/-/queries.csv data/RXR_alpha/-/queries.ecfp4
# head -1 data/PR/+/queries.csv        data/PR/+/queries.ecfp4
# head -1 data/PR/-/queries.csv        data/PR/-/queries.ecfp4
# head -1 data/ER_alpha/+/db.csv  ./data/ER_alpha/+/db.ecfp4
# head -1 data/ER_alpha/-/db.csv  ./data/ER_alpha/-/db.ecfp4
# head -1 data/RAR_alpha/-/db.csv ./data/RAR_alpha/-/db.ecfp4
# head -1 data/ER_beta/+/db.csv   ./data/ER_beta/+/db.ecfp4
# head -1 data/GR/+/db.csv        ./data/GR/+/db.ecfp4
# head -1 data/GR/-/db.csv        ./data/GR/-/db.ecfp4
# head -1 data/AR/-/db.csv        ./data/AR/-/db.ecfp4
# head -1 data/RXR_alpha/-/db.csv ./data/RXR_alpha/-/db.ecfp4
# head -1 data/PR/+/db.csv        ./data/PR/+/db.ecfp4
# head -1 data/PR/-/db.csv        ./data/PR/-/db.ecfp4
# tail -1 data/ER_alpha/+/queries.csv  data/ER_alpha/+/queries.ecfp4
# tail -1 data/ER_alpha/-/queries.csv  data/ER_alpha/-/queries.ecfp4
# tail -1 data/RAR_alpha/-/queries.csv data/RAR_alpha/-/queries.ecfp4
# tail -1 data/ER_beta/+/queries.csv   data/ER_beta/+/queries.ecfp4
# tail -1 data/GR/+/queries.csv        data/GR/+/queries.ecfp4
# tail -1 data/GR/-/queries.csv        data/GR/-/queries.ecfp4
# tail -1 data/AR/-/queries.csv        data/AR/-/queries.ecfp4
# tail -1 data/RXR_alpha/-/queries.csv data/RXR_alpha/-/queries.ecfp4
# tail -1 data/PR/+/queries.csv        data/PR/+/queries.ecfp4
# tail -1 data/PR/-/queries.csv        data/PR/-/queries.ecfp4
# tail -1 data/ER_alpha/+/db.csv  ./data/ER_alpha/+/db.ecfp4
# tail -1 data/ER_alpha/-/db.csv  ./data/ER_alpha/-/db.ecfp4
# tail -1 data/RAR_alpha/-/db.csv ./data/RAR_alpha/-/db.ecfp4
# tail -1 data/ER_beta/+/db.csv   ./data/ER_beta/+/db.ecfp4
# tail -1 data/GR/+/db.csv        ./data/GR/+/db.ecfp4
# tail -1 data/GR/-/db.csv        ./data/GR/-/db.ecfp4
# tail -1 data/AR/-/db.csv        ./data/AR/-/db.ecfp4
# tail -1 data/RXR_alpha/-/db.csv ./data/RXR_alpha/-/db.ecfp4
# tail -1 data/PR/+/db.csv        ./data/PR/+/db.ecfp4
# tail -1 data/PR/-/db.csv        ./data/PR/-/db.ecfp4

# tkdiff data/ER_alpha/+/queries.csv  data/ER_alpha/+/queries.ecfp4
# tkdiff data/ER_alpha/-/queries.csv  data/ER_alpha/-/queries.ecfp4
# tkdiff data/RAR_alpha/-/queries.csv data/RAR_alpha/-/queries.ecfp4
# tkdiff data/ER_beta/+/queries.csv   data/ER_beta/+/queries.ecfp4
# tkdiff data/GR/+/queries.csv        data/GR/+/queries.ecfp4
# tkdiff data/GR/-/queries.csv        data/GR/-/queries.ecfp4
# tkdiff data/AR/-/queries.csv        data/AR/-/queries.ecfp4
# tkdiff data/RXR_alpha/-/queries.csv data/RXR_alpha/-/queries.ecfp4
# tkdiff data/PR/+/queries.csv        data/PR/+/queries.ecfp4
# tkdiff data/PR/-/queries.csv        data/PR/-/queries.ecfp4
# tkdiff data/ER_alpha/+/db.csv  ./data/ER_alpha/+/db.ecfp4
# tkdiff data/ER_alpha/-/db.csv  ./data/ER_alpha/-/db.ecfp4
# tkdiff data/RAR_alpha/-/db.csv ./data/RAR_alpha/-/db.ecfp4
# tkdiff data/ER_beta/+/db.csv   ./data/ER_beta/+/db.ecfp4
# tkdiff data/GR/+/db.csv        ./data/GR/+/db.ecfp4
# tkdiff data/GR/-/db.csv        ./data/GR/-/db.ecfp4
# tkdiff data/AR/-/db.csv        ./data/AR/-/db.ecfp4
# tkdiff data/RXR_alpha/-/db.csv ./data/RXR_alpha/-/db.ecfp4
# tkdiff data/PR/+/db.csv        ./data/PR/+/db.ecfp4
# tkdiff data/PR/-/db.csv        ./data/PR/-/db.ecfp4
