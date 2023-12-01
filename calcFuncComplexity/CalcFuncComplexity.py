from .Log import newLog
from .dep.GetDependencyInfo import getDependencyInfo
from .dyn.DynamicAnalysis import runDynamicAnalysis

def calcCompl(funcListData):
    newLog()
    constrList = getDependencyInfo(funcListData)
    runDynamicAnalysis(constrList)
