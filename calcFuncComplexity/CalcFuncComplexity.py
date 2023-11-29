from .Log import newLog
from .dep.GetDependencyInfo import getDependencyInfo


def calcCompl(funcListData):
    newLog()
    getDependencyInfo(funcListData)
