from baseclass import BaseClass
import os
import subprocess

class TestCharCount(BaseClass):
  
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

  def test_charcount_url(self):
    """
    If you pass a string with the following conditions to _charcount.py, Make sure that no Markdown parsing is done.
    * Plain text.
    * No mdwc command.
    """
    p = os.environ["PATH"]
    try:
      os.environ["PATH"] = ';'.join(filter(lambda s: not r"\npm" in s, os.environ["PATH"].split(";")))
      try:
        self.assertNotRegex(self._checkscript("test", "run/_charcount"), r"Markdown")
      except subprocess.CalledProcessError: # CalledProcessError = mdwc not installed
        pass        
    finally:
      os.environ["PATH"] = p

