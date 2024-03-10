from dataclasses import dataclass
from typing import List, Tuple

import pandas as pd
from sklearn.linear_model import Lasso
from sklearn.model_selection import train_test_split

from .Data import Data


@dataclass
class RegressionResult:
    groupIdx: int
    y: str
    xTerms: List[Tuple[str, float]]  # (var, coef)
    xConst: float


def lassoRegression(data: Data) -> List[RegressionResult]:
    inputTrain, inputTest, outputTrain, outputTest = train_test_split(
        data.inputExtData, data.outputData, test_size=0.1, random_state=None
    )

    lassoModel = Lasso(alpha=0.1, fit_intercept=True)
    lassoModel.fit(inputTrain, outputTrain)

    coefs = lassoModel.coef_
    const = lassoModel.intercept_
    
    print(coefs)
    print(const)
