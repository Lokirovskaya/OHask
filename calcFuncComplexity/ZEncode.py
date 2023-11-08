encDict = {
    "z": "z",
    "!": "a",
    '"': "b",
    "#": "c",
    "$": "d",
    "%": "e",
    "&": "f",
    "'": "g",
    "(": "h",
    ")": "i",
    "*": "j",
    "+": "k",
    ",": "l",
    "-": "m",
    ".": "n",
    "/": "o",
    ":": "p",
    ";": "q",
    "<": "r",
    "=": "s",
    ">": "t",
    "?": "u",
    "@": "v",
    "[": "w",
    "\\": "x",
    "]": "y",
    "^": "0",
    "`": "1",
    "{": "2",
    "|": "3",
    "}": "4",
    "~": "5",
    " ": "6",
}


def reverseDict(d):
    result = {}
    for k, v in d.items():
        assert v not in result
        result[v] = k
    return result


decDict = reverseDict(encDict)


# escape ascii string into [0-9a-zA-Z_]+
def zEncode(s: str) -> str:
    result = ""
    for c in s:
        if c in encDict:
            result += "z" + encDict[c]
        else:
            result += c
    return result


def zDecode(s: str) -> str:
    if len(s) <= 1:
        return s

    result = ""
    i = 0
    while i < len(s) - 1:
        if s[i] == "z":
            assert s[i + 1] in decDict, f"Bad character `{s[i+1]}` in `{s}`"
            dec = decDict[s[i + 1]]
            result += dec
            i += 1
        else:
            result += s[i]
        i += 1
    return result
