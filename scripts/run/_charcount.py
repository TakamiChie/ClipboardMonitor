"""
[caption]
def=Counting characters
ja=文字数のカウント
[general]
order=100
"""

import sys, io, re, tempfile, subprocess

def colortags(m: re.Match) -> str:
  COLORS = [
    ["#000000", "#800000", "#008000", "#804000", "#000080", "#800080", "#008080"],
    ["#808080", "#ff0000", "#00ff00", "#ff8000", "#0000ff", "#ff00ff", "#00ffff"]
  ]
  n = int(m[1])
  t = m[2]
  ci = 0 if n / 10 == 3 else 1
  c = COLORS[ci][n % 10]
  return f'<span style="color: {c};">{t}</span>'

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()
  p = re.compile(r"\x1b\[([3,9][0-7])m(.*?)\x1b\[([3,9])9m")

  print(f"<p>{len(text) - 1} char(s)</p>")
  try:
    path = subprocess.check_output("where mdwc").decode("UTF-8").split()[-1]
    with tempfile.NamedTemporaryFile("w+t", encoding="utf-8") as f:
      f.write(text)
      # DONE: Reflect console color specification.
      ret = subprocess.check_output(f'"{path}" "{f.name}"').decode("UTF-8")
      ret = p.sub(colortags, ret)
      print('<p>Markdown</p>')
      print(f'<pre>{ret}</pre>')

  except:
    pass
