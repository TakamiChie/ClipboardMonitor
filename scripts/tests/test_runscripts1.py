import unittest
import io
import sys
from pathlib import Path

class TestRunScripts1(unittest.TestCase):

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

  def _checkscript(self, inputstr: str, scriptname: str) -> str:
    sys.stdin.write(f" {inputstr}")
    sys.stdin.seek(0)
    script = ""
    with open(Path(__file__).parent.parent / f"{scriptname}.py", "r", encoding="utf-8") as f:
      for s in f.readlines():
        if not s.startswith("sys.std"): script += f"{s}\n";
    exec(script)
    return sys.stdout.getvalue()

  def test_charcount_blank(self):
    """
    If you pass a string with the following conditions to _charcount.py, Confirm that it is returned as 0 characters.
    * No character string specified.
    """
    self.assertRegex(self._checkscript("", "run/_charcount"), r"0 char\(s\)")

  def test_charcount_string(self):
    """
    If you pass a string with the following conditions to _charcount.py, make sure that only the character count is returned.
    * String without URL
    """
    self.assertRegex(self._checkscript("test", "run/_charcount"), r"4 char\(s\)")

  def test_charcount_url(self):
    """
    If you pass a string with the following conditions to _charcount.py, make sure that only the character count is returned.
    * URL
    """
    self.assertRegex(self._checkscript("http://example.com/", "run/_charcount"), r"19 char\(s\)")