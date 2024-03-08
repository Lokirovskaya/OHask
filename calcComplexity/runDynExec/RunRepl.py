import subprocess as sp
from typing import List

from calcComplexity.runDynExec import Group

defaultTimes = 100


def runRepl(groups: List[Group]):
    tests = []
    
    with open("stat/dyn_result.txt", "w"):
        pass
    
    for g in groups:
        paramCount = len(g.paramVars)
        if paramCount >= 1 and paramCount <= 4:
            tests.append(
                f"testGroup{paramCount} {defaultTimes*paramCount} {g.groupIdx} {g.groupName}"
            )
        else:
            pass  # todo: show warning

    testStr = " >> ".join(tests)

    args = ["cabal", "repl", "DynExec", "-v0"]
    repl = sp.Popen(args, stdin=sp.PIPE)
    repl.communicate(input=testStr.encode())
    repl.wait()
