from baseclass import BaseClass
import os
import subprocess

class TestCBullet(BaseClass):
  
  FILE = "conversion/_bullet"
  
  def test_blank(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure that nothing is output.
    * No character string specified.
    """
    self.assertEqual(self._checkscript("", self.FILE), "")

  def test_oneline_text(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure you return the bulleted text.
    * One-line text.
    """
    self.assertEqual(self._checkscript("test", self.FILE), "* test\n")

  def test_multiline_text(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure you return the bulleted text.
    * Multi-line text.
    """
    self.assertMultiLineEqual(self._checkscript("abc\ndef\nghi\n", self.FILE), "* abc\n* def\n* ghi\n\n")

