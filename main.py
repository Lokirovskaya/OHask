#!/usr/bin/python3

import os
import sys
import shutil
import subprocess as sp
import datetime


# Read src path
if len(sys.argv) != 2:
    print("Missing argument: source file path")
    sys.exit(-1)

# Copy to run/Run.hs
src_path = sys.argv[1]
with open(src_path, "r") as f:
    src_content = f.read()

os.makedirs("run", exist_ok=True)
with open("run/Run.hs", "w") as f:
    cur_datetime = str(datetime.datetime.now())
    f.write("-- " + cur_datetime + "\n" + src_content)

# Run cabal
os.makedirs("stat", exist_ok=True)

sp.run(["cabal", "build"])
