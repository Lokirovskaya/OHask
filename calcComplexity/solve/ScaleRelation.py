from dataclasses import dataclass
from typing import List, Set

import sympy

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

    def getParamSymbols(self) -> Set[sympy.Symbol]:
        result = set()
        for _, rhs in self.constrList:
            result.update(rhs.atoms(sympy.Symbol))
        assert all(
            map(lambda p: p.name.startswith("p"), result)
        ), f"Corrupted param set {result}"
        return result
