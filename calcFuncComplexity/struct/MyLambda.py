from sympy import Lambda
from sympy.core.function import BadArgumentsError


# Extention of sympy.Lambda which supports partial-application


class MyLambda(Lambda):
    def __call__(self, *args):
        paramLen = list(self.nargs)[0]
        argLen = len(args)

        if argLen == paramLen:
            return super().__call__(*args)

        elif argLen < paramLen:
            params = super().variables
            expr = super().expr
            paramsComsumed = params[:argLen]
            paramsRemain = params[argLen:]
            return MyLambda(paramsRemain, Lambda(paramsComsumed, expr)(*args))

        else:
            raise BadArgumentsError(
                f"{self} takes {paramLen} argument(s) at most, but {argLen} given."
            )
