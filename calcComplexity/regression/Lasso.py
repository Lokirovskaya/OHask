from typing import List

from sklearn.linear_model import Lasso
from sklearn.model_selection import train_test_split

from .Data import Data
from .RegressionResult import RegressionResult
from calcComplexity.Log import logln

# hyperparams
lassoAlpha = 0.5
lassoIter = 2000
filterThresh = 0.01


def lassoRegression(data: Data) -> List[RegressionResult]:
    inputTrain, inputTest, outputTrain, outputTest = train_test_split(
        data.inputExtData, data.outputData, test_size=0.1, random_state=None
    )

    lassoModel = Lasso(alpha=lassoAlpha, fit_intercept=True, max_iter=lassoIter)
    lassoModel.fit(inputTrain, outputTrain)

    coefsList = lassoModel.coef_
    constList = lassoModel.intercept_

    inputVars = data.inputExtData.columns.to_list()
    outputVars = data.outputData.columns.to_list()

    # Get formatted result from regression result
    regResults = []

    def makeXTerms(vars_, coefs):
        return list(zip(vars_, coefs))

    if len(outputVars) == 1:
        assert isinstance(constList, float)
        regResults.append(
            RegressionResult(
                groupIdx=data.groupIdx,
                y=outputVars[0],
                xTerms=makeXTerms(inputVars, coefsList),
                xConst=constList,
            )
        )
    else:
        for i in range(len(outputVars)):
            oVar = outputVars[i]
            coefs = coefsList[i]
            const = constList[i]  # type: ignore
            regResults.append(
                RegressionResult(
                    groupIdx=data.groupIdx,
                    y=oVar,
                    xTerms=makeXTerms(inputVars, coefs),
                    xConst=const,
                )
            )

    list(map(filterSmallTerms, regResults))

    logln("[Regression Result]")
    for result in regResults:
        logln(str(result))
    logln()

    return regResults


def abs(x):
    return x if x >= 0.0 else -x


def filterSmallTerms(result: RegressionResult):
    newTerms = []
    for term in result.xTerms:
        if abs(term[1]) > filterThresh:
            newTerms.append(term)
    newTerms.sort(key=lambda t: t[0])  # Sort by var name
    result.xTerms = newTerms
