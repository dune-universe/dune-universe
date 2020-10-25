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

import json
from sys import float_info

def is_nan(f) : return f != f

def eq_float_eps(f1, f2, epsilon):
    abs1 = abs(f1)
    abs2 = abs(f2)
    diff = abs(f1 - f2)

    if f1 == f2:
        return True
    elif is_nan(f1):
        return is_nan(f2)
    elif is_nan(f2):
        return is_nan(f1)
    elif f1 == 0 or f2 == 0 or abs1 + abs2 < float_info.epsilon:
        return (diff < (epsilon * float_info.epsilon))
    else:
        return (diff / (min(abs1 + abs2, float_info.max)) < epsilon)

def eq_float(f1, f2):
    return eq_float_eps(f1, f2, 1e-10)

def forall2(fun, l1, l2):
    for i in range(len(l1)):
        if i >= len(l2): raise IndexError
        if not fun(l1[i], l2[i]):
            return False
    return True

def eq_structures(json1, json2, ignoredFields=[], verbose=False):
    if type(json1) == dict and type(json2) == dict:
        keys = list(json1.keys()) + list(json2.keys())
        keys = [k for k in keys if k not in ignoredFields]
        for key in keys:
            if key not in json1.keys() or key not in json2.keys():
                if verbose:
                    print("Field %s is not in one of the two structures" % key)
                return False
            if not eq_structures(json1[key], json2[key], ignoredFields):
                if verbose:
                    print("JSON at field %s" % key)
                    print(json1[key])
                    print("and")
                    print(json2[key])
                    print("are not equal")
                return False
        return True
    elif type(json1) == list and type(json2) == list:
        return forall2(eq_structures, json1, json2)
    elif type(json1) == str and type(json2) == str or\
         type(json1) == int and type(json2) == int or\
         type(json1) == bool and type(json2) == bool:
        return (json1 == json2)
    elif type(json1) == float and type(json2) == float:
        return eq_float(json1, json2)
    elif json1 is None and json2 is None:
        return True
    else:
        print("Unknown JSON type")
        assert False

def eq_list(eq_fun, l, **kwargs):
    first_elt = l[0]
    for i in range(1, len(l)):
        if not eq_fun(first_elt, l[i], **kwargs):
            return False
    return True

def eq_str_json_list(jsons_str, ignoredFields=None, verbose=False):
    ignored = [] if ignoredFields is None else ignoredFields
    jsons = list(map(json.loads, jsons_str))

    return eq_list(eq_structures, jsons, ignoredFields=ignored, verbose=verbose)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="check that all the given json \
    structures are equal, except for ignored fields")
    parser.add_argument('json', type=str, nargs='+', help='json structures')
    parser.add_argument('-v', dest="verbose", action="store_const",
                        default=False, const=True, help='verbose')
    parser.add_argument('-ignore', type=str, action='append',
                        help='ignore this field')

    args = parser.parse_args();

    if args.verbose:
        print("Ignored fields:")
        if args.ignore is not None:
            for ign in args.ignore:
                print("\t%s" % ign)
        print("JSONS:")
        for js in args.json:
            print("\t%s" % js)

    if eq_str_json_list(args.json, args.ignore, verbose=args.verbose):
        print("OK")
        exit(0)
    else:
        print("NOT OK")
        exit(1)
