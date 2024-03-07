#!/usr/bin/python3

import os
import sys
import subprocess as sp
import datetime

# Read args
# required: srcPath
# optional: --cabal
#           --compl

srcPath = ""
runCabal = False
runComplCalc = False
hasOptArg = False
for arg in sys.argv[1:]:
    if not arg.startswith("--"):
        srcPath = arg
    if arg == "--cabal":
        runCabal = True
        hasOptArg = True
    elif arg == "--compl":
        runComplCalc = True
        hasOptArg = True
if not hasOptArg:
    runCabal = True
    runComplCalc = True


RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
BOLD = "\x1b[1m"
END = "\x1b[0m"


if runCabal:
    # Copy to tmp/maintmp/MainTmp.hs
    if srcPath == "":
        print("Error: No src path provided.")
        exit(-1)

    with open(srcPath, "r") as f:
        srcContent = f.read()

    os.makedirs("tmp", exist_ok=True)
    os.makedirs("tmp/mainTmp", exist_ok=True)
    os.makedirs("tmp/dynExprs", exist_ok=True)

    with open("tmp/mainTmp/MainTmp.hs", "w") as f:
        curDatetime = str(datetime.datetime.now())
        f.write("-- " + curDatetime + "\n" )
        f.write("module MainTmp where\n")
        f.write(srcContent)

    # Clear essential files
    def createEmpty(path):
        with open(path, "w"):
            pass

    os.makedirs("stat", exist_ok=True)
    createEmpty("stat/calc_log.txt")
    createEmpty("stat/stat_brief.txt")
    createEmpty("stat/stat.json")
    createEmpty("stat/tree.txt")
    createEmpty("stat/dyn_result.txt")

    # Run cabal
    print(f"{BOLD}{YELLOW}=== Analysing Haskell File ==={END}")
    r = sp.run(["cabal", "build", "MainTmp"])

    if r.returncode != 0:
        print(f"{RED}Error Occurred ({r.returncode}){END}\n")
        exit(-1)
    else:
        print(f"{GREEN}Success{END}\n")


if runComplCalc:
    print(f"{BOLD}{YELLOW}=== Solving Complexity ==={END}")

    # calc func complexity
    # from calcFuncComplexity.CalcFuncComplexity import calcCompl
    # calcCompl(funcListData)
    import ujson
    from calcComplexity import calcComplexity

    with open("stat/stat.json", "r") as f:
        funcsData = ujson.load(f)
    calcComplexity(funcsData)

    print(f"{GREEN}Success{END}\n")
