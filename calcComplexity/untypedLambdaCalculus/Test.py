from Lambda import *
from Util import *

# Currying
print(currying([], Var("f")))
print(currying([Var("p")], Var("f")))
print(currying([Var("p1"), Var("p2")], Var("f")))
print(currying([Var("p1"), Var("p2"), Var("p3"), Var("p4"), Var("p5")], Var("f")))

# Uneval Subst
addf = Var("Add", isValue=False)
addxy = addf.app(Var("x"), Var("y"))
print(addxy)
