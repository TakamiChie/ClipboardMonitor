"""
[caption]
def=&Bullet(Markdown)
ja=箇条書きにする(Markdown記法)(&B)
"""

import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()

  if text != "":
    print(re.sub(r"^.+$", "* \g<0>", text, flags=re.M))

