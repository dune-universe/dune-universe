import re
from gmpy2 import *
import os.path
from qiskit.qasm import pi
import ast
import sys

''' 
Converts rzq(num, den) to rz((num/den)*pi). Overwrites input file.

We need to use this instead of VOQC's built-in "replace_rzq" function because
VOQC's version will convert e.g. rzq(5,4) to rz(3.926991) instead of rz((5/4)*pi).
These two values are not the same (due to rounding) and PyZX will fail to 
verfy their equality. This suggests that we should change VOQC's replace_rzq
function to treat PI as symbolic... or at least recognize the easy cases where
the parameter is a multiple of PI/4.
'''
def rzq_to_rz(fname_in):
    count =  0
    line1 = []
    p_rzq = re.compile("rzq\((.*),(.*)\) q\[([0-9]+)\];")
    with open(str(fname_in), 'r') as f:
        data = f.readlines()
        for line in data:
            m1 = p_rzq.match(line)
            if m1:
                line1.append(count)
            count = count+1
    for i in range(len(line1)):
        a = p_rzq.match(data[line1[i]])
        num1 = int(a.group(1))
        num2 = int(a.group(2))
        q = int(a.group(3))
        t= mpq(int(num1), int(num2))
        y = float(mpfr(t, 53))
        data[line1[i]] = "rz(%s*pi) q[%d];\n" % (y, q)
    with open(fname_in, 'w') as f:
        f.writelines(data)

rzq_to_rz(sys.argv[1])
