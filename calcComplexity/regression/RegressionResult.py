from dataclasses import dataclass
from typing import List, Tuple


@dataclass
class RegressionResult:
    groupIdx: int
    y: str
    xTerms: List[Tuple[str, float]]  # (var, coef)
    xConst: float

    def __str__(self) -> str:
        if len(self.xTerms) > 0:
            terms = []
            for t in self.xTerms:
                if t[1] >= 0.0:
                    terms.append(f"{t[1]:.3f}*{t[0]}")
                else:
                    terms.append(f"({t[1]:.3f})*{t[0]}")
            termsStr = " + ".join(terms)
            return f"group{self.groupIdx}: {self.y} = {termsStr} + {self.xConst:.3f}"
        else:
            return f"group{self.groupIdx}: {self.y} = {self.xConst:.3f}"

    def __repr__(self) -> str:
        return self.__str__()
