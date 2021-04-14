

from wscript_utils import *

def options(cfg):
    pass

def configure(cfg):
    pass

def build(bld):
    bld.recurse("hello-world")
    bld.recurse("double-vector")
    
def build3(bld):
    pass

def build4(bld):
    pass

