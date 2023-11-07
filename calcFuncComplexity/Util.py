from typing import Dict, Any

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
}


def reverseDict(d: Dict[Any, Any]) -> Dict[Any, Any]:
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
