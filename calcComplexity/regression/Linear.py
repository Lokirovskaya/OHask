from typing import Dict, List

from .RegressionResult import LassoResult, LinearResult
from .Data import Data


def linearRegression(
    datas: List[Data], lassoResults: List[LassoResult]
) -> List[LinearResult]:

    # Find data by groupIdx
    dataOf: Dict[int, Data] = {}

    for data in datas:
        dataOf[data.groupIdx] = data

    linearResults = []
    for lasso in lassoResults:
        data = dataOf[lasso.groupIdx]
        y = lasso.y
        xs = lasso.getXVars()
        linearResults.append(runLinear(data, y, xs))

    return linearResults


def runLinear(data: Data, y: str, xs: List[str]) -> LinearResult:
    pass
