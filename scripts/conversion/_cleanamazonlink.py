"""
[caption]
def=Clean amazon links.
ja=Amazonリンクをきれいにする
"""

import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()

if text != "":
  m = re.search(r"(https?://)([\.\w]*amazon\.[^/]*/).*(dp/\w+/).*", text)
  print(f"{m[1]}{m[2]}{m[3]}" if m else text)
