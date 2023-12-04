logFile = "stat/calc_log.txt"


def newLog():
    with open(logFile, "w"):
        pass


def log(s="", end="\n"):
    with open(logFile, "a") as f:
        f.write(str(s) + end)
