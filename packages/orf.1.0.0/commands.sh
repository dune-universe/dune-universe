

function ap_to_csv () {
    cut -d',' -f1,3 $1 > train
    sed -i 's/^/0 /g' train
    sed -i 's/^0 active/1 /g' train
    sed -i 's/,/ /g' train
    cut -d' ' -f1,3 train | tr -d '[' | tr -d ']' | tr ';' ' ' > $1.csv
}

ap_to_csv data/train.csv
ap_to_csv data/test.csv

