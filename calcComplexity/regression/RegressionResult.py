from dataclasses import dataclass
from typing import List, Tuple


@dataclass
class RegressionResult:
    groupIdx: int
    y: str
    xTerms: List[Tuple[str, float]]  # (var, coef)
    xConst: float

    def getXVars(self) -> List[str]:
        return list(map(lambda t: t[0], self.xTerms))

    def __str__(self) -> str:
        if self.xConst >= 0.0:
            constStr = f"{self.xConst:.3f}"
        else:
            constStr = f"({self.xConst:.3f})"

        if len(self.xTerms) > 0:
            terms = []
            for t in self.xTerms:
                if t[1] >= 0.0:
                    terms.append(f"{t[1]:.3f}*{t[0]}")
                else:
                    terms.append(f"({t[1]:.3f})*{t[0]}")
            termsStr = " + ".join(terms)
            return f"group{self.groupIdx}: {self.y} = {termsStr} + {constStr}"

        else:
            return f"group{self.groupIdx}: {self.y} = {constStr}"

    def __repr__(self) -> str:
        return self.__str__()


class LassoResult(RegressionResult):
    pass


@dataclass
class LinearResult(RegressionResult):
    r2: float

    def __str__(self) -> str:
        return super().__str__() + f"  (r2={self.r2:.4f})"
