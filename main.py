#!/usr/bin/python3

import os
import sys
import subprocess as sp
import datetime

# Read src path
if len(sys.argv) != 2:
    print("Missing argument: source file path.")
    sys.exit(-1)

# Copy to run/Run.hs
srcPath = sys.argv[1]
with open(srcPath, "r") as f:
    srcContent = f.read()

os.makedirs("run", exist_ok=True)

with open("run/Run.hs", "w") as f:
    curDatetime = str(datetime.datetime.now())
    f.write("-- " + curDatetime + "\n" + srcContent)

# Run cabal
os.makedirs("stat", exist_ok=True)

sp.run(["cabal", "build"])
print("\n")

# Read json
try:
    import ujson as json
except ImportError:
    print("Package ujson not found, use vanilla json.")
    import json

with open("stat/stat.json", "r") as f:
    funcListJson = json.load(f)

# calc func complexity
from calcFuncComplexity.Api import Func
from calcFuncComplexity.CalcFuncComplexity import calcCompl

funcComplList = list((map(Func, funcListJson)))
calcCompl(funcComplList)
