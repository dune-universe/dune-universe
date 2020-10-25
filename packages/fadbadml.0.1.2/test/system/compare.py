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

import sys
from subprocess import check_output
from random import random, randint
from compare_json import eq_str_json_list
from multiprocessing import Pool, Array
from time import sleep, time
from functools import partial

global_env = {}

def make_cmd(exe, *args, **kwargs):
    res = ["./%s" % exe]
    for arg in args:
        res += [arg]
    for k,v in kwargs.items():
        res += ["-%s" % k, str(v)]
    return res

def get_str_jsons(nsteps, dt):
    cmds = map(lambda exe: make_cmd(exe, n=nsteps, dt=dt), global_env["exe"])
    outs = map(check_output, cmds)
    return map(lambda b: b.decode("utf8"), outs)

def compare_once(nsteps, dt):
    jsons = list(get_str_jsons(nsteps, dt))
    try:
        ok = eq_str_json_list(jsons, ["exec_time"])
    except Exception as e:
        return jsons, False

    return jsons, ok

def compare_(id, ntests, mindt=0., maxdt=1., minsteps=0, maxsteps=2,
             verbose=False, **kwargs):
    starttime = time()
    padsize = len(str(ntests))
    for i in range(ntests):
        global_env["progress"][id] = i
        if verbose:
            cur_time = time() - starttime;
            print("testing: | %*d/%d | (approx time: %.2fs)" %\
                (padsize, i, ntests, cur_time), end="\r", file=sys.stderr)
        nsteps = randint(minsteps, maxsteps)
        dt = random() * (maxdt - mindt) + mindt

        jsons, ok = compare_once(nsteps, dt)

        if not ok:
            if verbose:
                print(file=sys.stderr)
                print("Failure with JSONs:", file=sys.stderr)
                for js in jsons:
                    print("\t%s" % js, file=sys.stderr)
            return jsons, False
    global_env["progress"][id] = ntests
    if verbose: print(file=sys.stderr)
    return [], True

def compare(n, **kwargs): return compare_(0, n, verbose=True, **kwargs)

def compare_parallel(ntests, nprocesses=4, **kwargs):
    quotient = int(ntests / nprocesses)
    remainder = ntests % nprocesses
    total_runs = [quotient]*nprocesses
    for i in range(remainder):
        total_runs[i] += 1
    padsize = len(str(total_runs[0]))

    with Pool(nprocesses) as p:
        args = [(i, total_runs[i]) for i in range(nprocesses)]
        res = p.starmap_async(partial(compare_, **kwargs), args)
        count = 0
        while True:
            res.wait(0)
            if res.ready():
                break
            else:
                cur_count = ""
                for i in range(nprocesses):
                    cur_count += "%*d/%d | " %\
                        (padsize, global_env["progress"][i], args[i][1])
                print("testing: |", cur_count, "(approx time: %.2fs)" %\
                        (count / 10), end="\r", file=sys.stderr)
            count += 1
            sleep(0.1)
        p.close()
        p.join()
    cur_count = ""
    for i in range(nprocesses):
        cur_count += "%*d/%d | " %\
            (padsize, global_env["progress"][i], args[i][1])
    print("testing: |", cur_count, "(approx time: %.2fs)" %\
            (count / 10), end="\r", file=sys.stderr)
    for (jsons, ok) in res.get():
        if not ok:
            print(file=sys.stderr)
            print("Failure with JSONs:", file=sys.stderr)
            for js in jsons:
                print("\t%s" % js, file=sys.stderr)
            return jsons, False
    print(file=sys.stderr)
    return [], True

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="runs fadcpp and fadml, \
    badcpp and badml or tadcpp and tadml with the same arguments nstep and dt \
    and checks that their results are equal")
    parser.add_argument('prog', type=str,
                        help='prog to test : fad, bad, fad_bad or tad')
    parser.add_argument('-n', '-ntests', type=int, default=50,
                        help='number of tests to run')
    parser.add_argument('-j', '-nprocesses', type=int, default=8,
                        help='number of processes to spawn')
    parser.add_argument('-minsteps', type=int, default=0,
                        help='minimal number of steps')
    parser.add_argument('-maxsteps', type=int, default=2,
                        help='maximal number of steps')
    parser.add_argument('-mindt', type=float, default=0.,
                        help='minimal size of step')
    parser.add_argument('-maxdt', type=float, default=1.,
                        help='maximal size of step')
    args = parser.parse_args();

    global_env["progress"] = Array('i', args.j)

    kwargs = { "ntests": args.n, "nprocesses": args.j,
               "minsteps": args.minsteps, "maxsteps": args.maxsteps,
               "mindt": args.mindt, "maxdt": args.maxdt }

    print("Options:", kwargs, file=sys.stderr)

    if args.prog == "fad":
        global_env["exe"] = ["fadcpp.exe", "fadml.exe"]
    elif args.prog == "bad":
        global_env["exe"] = ["badcpp.exe", "badml.exe"]
    elif args.prog == "tad":
        global_env["exe"] = ["tadcpp.exe", "tadml.exe"]
    elif args.prog == "fad_bad":
        global_env["exe"] = ["fadcpp.exe", "fadml.exe", "badcpp.exe", "badml.exe"]
    else:
        print("Unknown value '%s' for argument prog" % args.prog, file=sys.stderr)
        parser.print_help(sys.stderr)

    if kwargs["nprocesses"] == 1:
        _, ok = compare(**kwargs)
    else:
        _, ok = compare_parallel(**kwargs)
    if ok:
        print("OK")
        exit(0)
    else:
        print("NOT OK")
        exit(1)
