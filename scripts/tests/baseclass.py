from typing import Any
import unittest
import io
import sys
from pathlib import Path

class BaseClass(unittest.TestCase):

  def setUp(self) -> None:
    self.naturestdin = sys.stdin
    self.naturestdout = sys.stdout
    sys.stdin = io.StringIO()
    sys.stdout = io.StringIO()
    return super().setUp()

  def tearDown(self) -> None:
    sys.stdin = self.naturestdin
    sys.stdout = self.naturestdout
    return super().tearDown()

  def _checkscript(self, inputstr: str, scriptname: str, parameters: dict[str, Any] = None) -> str:
    sys.stdin.write(f"{inputstr}")
    sys.stdin.seek(0)
    script = ""
    with open(Path(__file__).parent.parent / f"{scriptname}.py", "r", encoding="utf-8") as f:
      for s in f.readlines():
        if not s.startswith("sys.std"): script += f"{s}\n";
    exec(script, globals() if parameters is None else parameters)
    return sys.stdout.getvalue()
