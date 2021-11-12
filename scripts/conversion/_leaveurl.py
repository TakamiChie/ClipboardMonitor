"""
[caption]
def=Erase everything but the URL
ja=URL以外を消す
"""

import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()

  m = re.search(r"(https?)(:\/\/[\w\/:%#\$&\?\(\)~\.=\+\-]+)", text)
  print(m[0] if m else text)
