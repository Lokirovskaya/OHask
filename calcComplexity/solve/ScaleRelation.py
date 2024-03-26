from dataclasses import dataclass
from typing import List

from calcComplexity.constraint import SympyConstraint

@dataclass
class ScaleRelation:
    name: str
    constrList: List[SympyConstraint]

    def __str__(self) -> str:
        ans = self.name + ":\n"
        for constr in self.constrList:
            ans += f"  {constr}\n"
        return ans[:-1]