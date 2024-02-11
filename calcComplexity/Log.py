LOG_PATH = "stat/calc_log.txt"
f = open(LOG_PATH, "w")


def logln(s=""):
    f.write(s + "\n")
