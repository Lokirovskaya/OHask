from typing import Dict, List

import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.metrics import r2_score

from calcComplexity.Log import logln

from .Data import Data
from .RegressionResult import LassoResult, LinearResult


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
        c = lasso.xConst
        result = runLinear(data, y, xs, c)
        linearResults.append(result)

    logln("[Linear Result]")
    for result in linearResults:
        logln(str(result))
    logln()

    return linearResults


def runLinear(data: Data, y: str, xs: List[str], c: float) -> LinearResult:
    # No input var selected
    if len(xs) == 0:
        return LinearResult(
            groupIdx=data.groupIdx,
            y=y,
            xTerms=[],
            xConst=c,
            r2=1.0,
        )

    yDf = data.outputData[y]
    xDf = data.inputExtData[xs]

    linearModel = LinearRegression(fit_intercept=True)
    linearModel.fit(xDf, yDf)

    coefs = linearModel.coef_
    const = linearModel.intercept_
    
    yTrue = yDf
    yPred = np.dot(xDf, coefs) + const
    r2 = r2_score(yTrue, yPred)
    
    def makeXTerms(vars_, coefs):
        return list(zip(vars_, coefs))

    result = LinearResult(
        groupIdx=data.groupIdx,
        y=y,
        xTerms=makeXTerms(xs, coefs),
        xConst=const,
        r2=r2,  # type: ignore
    )

    return result
