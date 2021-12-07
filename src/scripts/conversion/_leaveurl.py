"""
[caption]
def=Leave a &URL
ja=URL以外を消す
"""

import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()

if text != "":
  m = re.search(r"(https?)(:\/\/[\w\/:%#\$&\?\(\)~\.=\+\-]+)", text)
  print(m[0] if m else text)
