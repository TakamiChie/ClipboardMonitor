from baseclass import BaseClass
import os
import subprocess

class TestRCharCount(BaseClass):
  
  FILE = "run/_charcount"
  
  def test_charcount_blank(self):
    """
    If you pass a string with the following conditions to _charcount.py, Confirm that it is returned as 0 characters.
    * No character string specified.
    """
    self.assertEqual(self._checkscript("", self.FILE).split("\n")[0], "<p>0 char(s)</p>")

  def test_charcount_string(self):
    """
    If you pass a string with the following conditions to _charcount.py, make sure that only the character count is returned.
    * String without URL
    """
    self.assertEqual(self._checkscript("test", self.FILE).split("\n")[0], "<p>4 char(s)</p>")

  def test_charcount_url(self):
    """
    If you pass a string with the following conditions to _charcount.py, make sure that only the character count is returned.
    * URL
    """
    self.assertEqual(self._checkscript("http://example.com/", self.FILE).split("\n")[0], "<p>19 char(s)</p>")

  def test_charcount_mdwc_not_installed(self):
    """
    If you pass a string with the following conditions to _charcount.py, Make sure that no Markdown parsing is done.
    * Plain text.
    * No mdwc command.
    """
    p = os.environ["PATH"]
    try:
      os.environ["PATH"] = ';'.join(filter(lambda s: not r"\npm" in s, os.environ["PATH"].split(";")))
      try:
        self.assertEqual(self._checkscript("test", self.FILE).split("\n")[1], "<p>Markdown</p>")
      except subprocess.CalledProcessError: # CalledProcessError = mdwc not installed
        pass        
    finally:
      os.environ["PATH"] = p

