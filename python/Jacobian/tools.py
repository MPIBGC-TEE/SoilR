#!/usr/bin/python3
# vim:set ff=unix expandtab ts=4 sw=4:
import sys
import traceback
def who_am_i():
    stack = traceback.extract_stack()
    filename, codeline, funcName, text = stack[-2]
    return funcName
