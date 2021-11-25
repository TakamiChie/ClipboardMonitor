from baseclass import BaseClass

class TestRColorCode(BaseClass):
  
  FILE = "run/_colorcode"

  def test_nocolorcode(self):
    """
    If a character string is specified under the following conditions, check that the character string with the color code is not output.
    * A string that is not a color code.
    """
    self.assertEqual(self._checkscript("#nocolor", self.FILE), "")

  def test_colorcode(self):
    """
    If a character string is specified under the following conditions, check that the character string with the color code is output.
    * 6-digit color code.
    """
    self.assertEqual(self._checkscript("#80ABEF", self.FILE).split('\n')[0], '<p>Color:<span style="color:#80ABEF;">#80ABEF</span></p>')

  def test_colorcode_3digit(self):
    """
    If a character string is specified under the following conditions, check that the character string with the color code is output.
    * 3-digit color code.
    """
    self.assertEqual(self._checkscript("#8AF", self.FILE).split('\n')[0], '<p>Color:<span style="color:#8AF;">#8AF</span></p>')

  def test_colorcode_4digit(self):
    """
    If a character string is specified under the following conditions, check that the character string with the color code is not output.
    * 4-digit color code.
    """
    self.assertEqual(self._checkscript("#AABB", self.FILE), "")
