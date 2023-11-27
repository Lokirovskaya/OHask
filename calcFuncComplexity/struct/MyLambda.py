from sympy import Lambda, sympify, Expr
from sympy.core.function import BadArgumentsError
from sympy.utilities.iterables import iterable
from sympy.core.containers import Tuple


# Extention of sympy.Lambda in order to support partial-application
# Please ensure: Each param of lambda should be either Symbol or Function, not a complex expr


class MyLambda(Lambda):
    # Function param extension
    # Basically copied from Lambda.__new__
    def __new__(cls, signature, expr):
        if iterable(signature) and not isinstance(signature, (tuple, Tuple)):
            # sympy_deprecation_warning(
            #     """
            #     Using a non-tuple iterable as the first argument to Lambda
            #     is deprecated. Use Lambda(tuple(args), expr) instead.
            #     """,
            #     deprecated_since_version="1.5",
            #     active_deprecations_target="deprecated-non-tuple-lambda",
            # )
            # signature = tuple(signature)
            assert False

        sig = signature if iterable(signature) else (signature,)
        sig = sympify(sig)

        # cls._check_signature(sig)

        # if len(sig) == 1 and sig[0] == expr:
        # return S.IdentityFunction

        return Expr.__new__(cls, sig, sympify(expr))

    # Partial-application extension
    def __call__(self, *args):
        params = self.signature
        paramLen = len(params)
        argLen = len(args)

        if argLen == paramLen:
            # return super().__call__(*args)
            e = self.expr
            for param, arg in zip(params, args):
                e = e.replace(param, arg)
            return e

        elif argLen < paramLen:
            params = super().variables
            expr = super().expr
            paramsComsumed = params[:argLen]
            paramsRemain = params[argLen:]
            return MyLambda(paramsRemain, MyLambda(paramsComsumed, expr)(*args))

        else:
            raise BadArgumentsError(
                f"{self} takes {paramLen} argument(s) at most, but {argLen} given."
            )
    
    # Fix bug which raises exception when holding functional lambda params
    def replace(self, query, value, **kwargs):
        sig = list(self.signature)
        for i in range(len(sig)):
            if sig[i] == query:
                sig[i] = value
        sig = tuple(sig)
        return MyLambda(sig, self.expr.replace(query, value, **kwargs))
