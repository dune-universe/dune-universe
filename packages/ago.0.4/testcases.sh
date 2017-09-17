#! /bin/sh
#
# Generate test cases, one per line. Each line consists of two dates and
# the number of days they are apart: 
#
# 1987-09-18,2003-10-24,5880.0
#


sql() 
{
    echo ".mode csv"
    jot -r 400 2420000 2480000 | while read x; do
        read y
        printf 'select  strftime("%%Y-%%m-%%d", julianday(%s)),
                        strftime("%%Y-%%m-%%d", julianday(%s)),
                        julianday(%s) - julianday(%s); \n' $x $y $y $x
    done                    
}

sql | sqlite3
