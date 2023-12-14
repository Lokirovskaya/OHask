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
if srcPath == "":
    print("Error: No src path provided.")
    exit(-1)
if not hasOptArg:
    runCabal = True
    runComplCalc = True


RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
BOLD = "\x1b[1m"
END = "\x1b[0m"


if runCabal:
    # Copy to run/Run.hs
    srcPath = sys.argv[1]
    with open(srcPath, "r") as f:
        srcContent = f.read()

    os.makedirs("run", exist_ok=True)

    with open("run/Run.hs", "w") as f:
        curDatetime = str(datetime.datetime.now())
        f.write("-- " + curDatetime + "\n" + srcContent)

    # Clear essential files
    def createEmpty(path):
        with open(path, "w"):
            pass

    os.makedirs("stat", exist_ok=True)
    createEmpty("stat/calc_log.txt")
    createEmpty("stat/imports.txt")
    createEmpty("stat/stat_brief.txt")
    createEmpty("stat/stat.json")
    createEmpty("stat/tree.txt")

    # Run cabal
    print(f"{BOLD}{YELLOW}=== Analysing Haskell File ==={END}")
    r = sp.run(["cabal", "build", "Stat"])

    if r.returncode != 0:
        print(f"{RED}Error Occurred ({r.returncode}){END}\n")
        exit(-1)
    else:
        print(f"{GREEN}Success{END}\n")


if runComplCalc:
    print(f"{BOLD}{YELLOW}=== Solving Complexity ==={END}")

    # Read json
    try:
        import ujson as json
    except ImportError:
        print("Package ujson not found, use vanilla json.")
        import json

    with open("stat/stat.json", "r") as f:
        funcListData = json.load(f)

    # calc func complexity
    from calcFuncComplexity.CalcFuncComplexity import calcCompl

    calcCompl(funcListData)

    print(f"{GREEN}Success{END}\n")
