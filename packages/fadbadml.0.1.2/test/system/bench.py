#!/usr/bin/env python3

##########################################################################
#                                                                        #
#                                FADBADml                                #
#                                                                        #
#           OCaml port by François Bidet and Ismail Bennani              #
#     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      #
#                                                                        #
#                          Copyright 2019-2020                           #
#                                                                        #
#   This file is distributed under the terms of the CeCILL-C license.    #
#                                                                        #
##########################################################################

import sys, json, time
from subprocess import check_output
from multiprocessing import Pool
from functools import partial

global_env = {}

def make_cmd(exe, *args, **kwargs):
    res = ["./%s" % exe]
    for arg in args:
        res += [arg]
    for k,v in kwargs.items():
        res += ["-%s" % k, str(v)]
    return res

def get_exec_time(exe, nsteps):
    cmd = make_cmd(exe, n=nsteps, dt=global_env["dt"])
    outs = [check_output(cmd) for i in range(global_env["ntests"])]
    js = [json.loads(out) for out in outs]
    exec_times = [float(j["exec_time"]) for j in js]
    return sum(exec_times)/len(exec_times)

def make_result(nsteps, exec_times):
    ratio = float(exec_times[0]) / float(exec_times[1]) \
                if float(exec_times[1]) != 0 else 0
    res = [nsteps, global_env["dt"], exec_times[0], exec_times[1], ratio]
    if len(exec_times) > 2:
        ratio2 = float(exec_times[2]) / float(exec_times[3]) \
            if float(exec_times[3]) != 0 else 0
        res += [exec_times[2], exec_times[3], ratio2]
    return res

def bench_once(id, nsteps):
    exec_times = [get_exec_time(prog, nsteps)
                        for prog in global_env["progs"]]
    return make_result(nsteps, exec_times)

def star_apply(f, args): return f(*args)

def print_result(res):
    progs = [prog.replace('_', '-') for prog in global_env["progs"]]
    firstline = "nsteps\tdt\t%s\t%s\t%s/%s" % \
        (progs[0], progs[1], progs[0], progs[1])
    if len(progs) > 2:
        firstline += "\t%s\t%s\t%s/%s" % \
            (progs[2], progs[3], progs[2], progs[3])

    print(firstline)
    for line in res:
        print("%d\t%s" % (line[0], "\t".join(["%f" % f for f in line[1:]])))

def plot_result(res):
    res = list(res)
    if len(res) == 0: return

    from matplotlib import pyplot as plt
    import numpy as np

    progs = [prog.replace('_', '-') for prog in global_env["progs"]]

    nameX = "nsteps"
    nameY1 = progs[0]
    nameY2 = progs[1]
    nameratio = "%s/%s" % (nameY1, nameY2)

    X = np.array([l[0] for l in res])
    Y1 = np.array([l[2] for l in res])
    Y2 = np.array([l[3] for l in res])
    ratio = np.array([l[4] for l in res])
    plt.plot(X,ratio,label=nameratio)

    if len(res[0]) > 5:
        nameY3 = progs[2]
        nameY4 = progs[3]
        nameratio2 = "%s/%s" % (nameY3, nameY4)
        Y3 = np.array([l[5] for l in res])
        Y4 = np.array([l[6] for l in res])
        ratio2 = np.array([l[7] for l in res])
        plt.plot(X,ratio2,label=nameratio2)
    plt.legend()
    plt.show()

def bench(startn, stepn, endn, j, plot, **kwargs):
    all_ns = range(startn, endn, stepn)
    inputs = list(enumerate(all_ns))
    with Pool(j) as p:
        res = p.imap(partial(star_apply, bench_once), inputs)
        count = 0
        while res._index < len(all_ns):
            dots = '.' * (count % 4)
            print("running %-*s %d/%d" % (3, dots, res._index, len(all_ns)),
                  end="\r", file=sys.stderr)
            count += 1
            time.sleep(0.1)
        p.close()
        p.join()
    print(file=sys.stderr)

    if plot:
        plot_result(res)
    else:
        print_result(res)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="run one of fad, bad or tad\
        with different values for nsteps in OCaml and C++ and writes\
        the result to stdout (or plots them). For each nsteps the execution \
        times are averaged over `ntests` runs.")
    parser.add_argument('prog', type=str,
                        help='should be either fad, bad, fad_bad or tad')
    parser.add_argument('-ntests', type=int, default=10,
                        help='number of tests to run for each input')
    parser.add_argument('-startn', type=int, default=0,
                        help='first value of nsteps')
    parser.add_argument('-stepn', type=int, default=1,
                        help='steps between two values of nsteps')
    parser.add_argument('-endn', type=int, default=10,
                        help='last value of nsteps')
    parser.add_argument('-dt', type=float, default=0.01,
                        help='size of one step')
    parser.add_argument('-plot', action='store_const',
                        const=True, default=False,
                        help='plot the result instead of printing it')
    parser.add_argument('-j', type=int, default=4,
                        help='number of processes to spawn')
    args = parser.parse_args();

    if args.prog == 'fad':
        global_env["progs"] = ["fadml", "fadcpp"]
    elif args.prog == 'bad':
        global_env["progs"] = ["badml", "badcpp"]
    elif args.prog == 'fad_bad':
        global_env["progs"] = ["fadml", "fadcpp", "badml", "badcpp"]
    elif args.prog == 'tad':
        global_env["progs"] = ["tadml", "tadcpp"]
    else:
        print("Unknown value '%s' for argument prog" % args.prog, file=sys.stderr)
        parser.print_help(sys.stderr)

    global_env["dt"] = args.dt
    global_env["ntests"] = args.ntests

    kwargs = { 'startn': args.startn, 'stepn': args.stepn, 'endn': args.endn,
               'j': args.j, 'plot': args.plot}

    bench(**kwargs)
