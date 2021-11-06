import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()
  m =  re.match(r"^#([0-9a-f]{6}|[0-9a-f]{3})$", text, re.IGNORECASE)
  if m: print(f'<p>Color:<span style="color:#{m[1]};">#{m[1]}</span></p>')
