
# # full AP dictionary encoding
# molenc.sh --no-std --pairs -d data/chembl_24_AP.dix -i data/solubility_train_std.smi -o data/solubility_train_std.smi.AP
# molenc.sh --no-std --pairs -d data/chembl_24_AP.dix -i data/solubility_test_std.smi -o data/solubility_test_std.smi.AP

# # put back target values
# sed -e 's/,0\.0,/,/g' -e 's/^/x,/g' -i data/solubility_train_std.smi.AP
# sed -e 's/,0\.0,/,/g' -e 's/^/x,/g' -i data/solubility_test_std.smi.AP

# # convert to CSV
# molenc_dense -n 17368 -i data/solubility_train_std.smi.AP > data/solubility_train_std.smi.AP.csv
# molenc_dense -n 17368 -i data/solubility_test_std.smi.AP > data/solubility_test_std.smi.AP.csv

# # find ncomp_best --> 13
# oplsr_model -p 1.0 --train data/solubility_train_std.smi.AP.csv --NxCV 5 -np 8

# # train and save production model
# oplsr_model -p 1.0 --train data/solubility_train_std.smi.AP.csv --ncomp 13 -s ncomp13_AP.model

# # train and test model
# time oplsr_model -l ncomp13_AP.model --test data/solubility_test_std.smi.AP.csv --ncomp 13
# R2=0.86 dt=2.54s speed=98.82 molecule/s

# # dataset file size
# ls -lh data/solubility_train_std.smi.AP.csv data/solubility_test_std.smi.AP.csv
# 8.5M May 26 12:59 data/solubility_test_std.smi.AP.csv
# 34M May 26 12:59 data/solubility_train_std.smi.AP.csv

# # shrink model --> can drop 16384
# oplsr_model --shrink --ncomp 13 -l ncomp13_AP.model --train data/solubility_train_std.smi.AP.csv -np 8

# # list dropped features
#oplsr_model --ncomp 13 -l ncomp13_AP.model --drop-fn ncomp13_AP.model.dropped --drop 16384 --train data/solubility_train_std.smi.AP.csv

# # prune dictionary
# molenc_prune -i data/chembl_24_AP.dix -o data/chembl_24_AP.reduced.dix -f ncomp13_AP.model.dropped

# # encode using reduced dictionary
# molenc.sh --no-std --pairs -d data/chembl_24_AP.reduced.dix -i data/solubility_train_std.smi -o data/solubility_train_std.smi.reduced.AP
# molenc.sh --no-std --pairs -d data/chembl_24_AP.reduced.dix -i data/solubility_test_std.smi -o data/solubility_test_std.smi.reduced.AP

# # put back target values
# sed -e 's/,0\.0,/,/g' -e 's/^/x,/g' -i data/solubility_train_std.smi.reduced.AP
# sed -e 's/,0\.0,/,/g' -e 's/^/x,/g' -i data/solubility_test_std.smi.reduced.AP

# # convert to CSV
# molenc_dense -n 984 -i data/solubility_train_std.smi.reduced.AP > data/solubility_train_std.smi.reduced.AP.csv
# molenc_dense -n 984 -i data/solubility_test_std.smi.reduced.AP > data/solubility_test_std.smi.reduced.AP.csv

# # find ncomp_best --> 14
# oplsr_model -p 1.0 --train data/solubility_train_std.smi.reduced.AP.csv --NxCV 5 -np 8

# # train and save production model
# oplsr_model -p 1.0 --train data/solubility_train_std.smi.reduced.AP.csv --ncomp 13 -s ncomp13_AP.reduced.model

# # train and test model
# time oplsr_model -l ncomp13_AP.reduced.model --test data/solubility_test_std.smi.reduced.AP.csv --ncomp 13
# R2=0.86 dt=0.487s speed=515.40 molecule/s

# # dataset file size; screening speed
# # ls -lh data/solubility_train_std.smi.reduced.AP.csv data/solubility_test_std.smi.reduced.AP.csv
# 489K May 26 14:33 data/solubility_test_std.smi.reduced.AP.csv
# 1.9M May 26 14:33 data/solubility_train_std.smi.reduced.AP.csv

# # in summary:
# training set size: 5.9 times smaller
# test set size: 6.17 times smaller
# screening speed: 5.21 times faster
# model quality: unchanged
