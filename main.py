#!/usr/bin/python3

import os
import sys
import subprocess as sp
import datetime

# Read args
#   --stat      Collect program info only. Source file path required.
#   --dyn       Dynamic analysis only, based on program info above.
#   --solve     Solve complexity only, based on dyn analysis result above.
# If non of args provided, all processes will be executed.

srcPath = ""
runStat = False
runDyn = False
runSolve = False

for arg in sys.argv[1:]:
    if not arg.startswith("--"):
        srcPath = arg
    if arg == "--stat":
        runStat = True
    elif arg == "--dyn":
        runDyn = True
    elif arg == "--solve":
        runSolve = True

if not runStat and not runDyn and not runSolve:
    runStat = True
    runDyn = True
    runSolve = True


RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
BOLD = "\x1b[1m"
END = "\x1b[0m"


# Clear essential files
def createEmpty(path):
    with open(path, "w"):
        pass


if runStat:
    # Copy src file to tmp/maintmp/MainTmp.hs
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
        f.write("-- " + curDatetime + "\n")
        f.write("module MainTmp where\n")
        f.write(srcContent)

    os.makedirs("stat", exist_ok=True)
    createEmpty("stat/calc_log.txt")
    createEmpty("stat/stat_brief.txt")
    createEmpty("stat/stat.json")
    createEmpty("stat/tree.txt")

    # Run cabal
    print(f"{BOLD}{YELLOW}=== Analysing Haskell Program ==={END}")
    r = sp.run(["cabal", "build", "MainTmp"])

    if r.returncode != 0:
        print(f"{RED}Error Occurred ({r.returncode}){END}\n")
        exit(-1)
    else:
        print(f"{GREEN}Success{END}\n")


if runDyn or runSolve:
    from calcComplexity import calcComplexity
    calcComplexity(runDyn, runSolve)

    
