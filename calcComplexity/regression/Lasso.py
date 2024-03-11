from typing import List
import warnings

from sklearn.exceptions import ConvergenceWarning
from sklearn.linear_model import Lasso

from calcComplexity.Log import logln

from .Data import Data
from .RegressionResult import LassoResult


# hyperparams
lassoAlpha = 1.0
lassoIter = 1000
filterThresh = 0.05

warnings.filterwarnings(action="ignore", category=ConvergenceWarning)


def lassoRegression(datas: List[Data]) -> List[LassoResult]:
    results = []
    for data in datas:
        results += lassoOneData(data)

    logln("[Lasso Result]")
    for result in results:
        logln(str(result))
    logln()

    return results


def lassoOneData(data: Data) -> List[LassoResult]:
    lassoModel = Lasso(
        alpha=lassoAlpha,
        fit_intercept=True,
        max_iter=lassoIter,
        tol=0.01,
        random_state=111,
    )

    lassoModel.fit(data.inputExtData, data.outputData)

    coefsList = lassoModel.coef_
    constList = lassoModel.intercept_

    inputVars = data.inputExtData.columns.to_list()
    outputVars = data.outputData.columns.to_list()

    # Get formatted result from regression result
    regResults = []

    def makeXTerms(vars_, coefs):
        return list(zip(vars_, coefs))

    for i in range(len(outputVars)):
        oVar = outputVars[i]
        coefs = coefsList[i]
        const = constList[i]  # type: ignore
        regResults.append(
            LassoResult(
                groupIdx=data.groupIdx,
                y=oVar,
                xTerms=makeXTerms(inputVars, coefs),
                xConst=const,
            )
        )

    # Pipe
    list(map(filterSmallTerms, regResults))
    regResults.sort(key=lambda r: r.y)

    return regResults


def abs(x):
    return x if x >= 0.0 else -x


def filterSmallTerms(result: LassoResult):
    newTerms = []
    for term in result.xTerms:
        if abs(term[1]) > filterThresh:
            newTerms.append(term)
    newTerms.sort(key=lambda t: t[0])  # Sort by var name
    result.xTerms = newTerms
