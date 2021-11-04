import sys, io, tempfile, subprocess

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()

  print(f"<p>{len(text)} char(s)</p>")
  try:
    path = subprocess.check_output("where mdwc").decode("UTF-8").split()[-1]
    with tempfile.NamedTemporaryFile("w") as f:
      f.write(text)
      ret = subprocess.check_output(f'"{path}" "{f.name}"').decode("UTF-8")
      print('<p>Markdown<p>')
      print(f'<pre>{ret}</pre>')

  except:
    pass
