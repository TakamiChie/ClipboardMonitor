from baseclass import BaseClass
import os
import subprocess

class TestCLeaveURL(BaseClass):
  
  FILE = "conversion/_leaveurl"
  
  def test_blank(self):
    """
    If you pass a string with the following conditions to _leaveurl.py, Make sure that nothing is output.
    * No character string specified.
    """
    self.assertEqual(self._checkscript("", self.FILE), "")

  def test_no_url(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the string is returned as it is.
    * A string that does not contain a URL.
    """
    self.assertEqual(self._checkscript("testtesttest", self.FILE), "testtesttest\n")

  def test_url_only(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure to return only the URL.
    * URL only.
    """
    self.assertEqual(self._checkscript("http://example.com/", self.FILE), "http://example.com/\n")

  def test_https_url(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure to return only the URL.
    * HTTPS URL only.
    """
    self.assertEqual(self._checkscript("https://example.com/", self.FILE), "https://example.com/\n")

  def test_contains_url(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure you return the bulleted text.
    * Contains the URL.
    """
    self.assertEqual(self._checkscript("testtesttest http://example.com/ testtesttest", self.FILE), "http://example.com/\n")


  def test_contains_url_multilinetext(self):
    """
    If you pass a string with the following conditions to _bullet.py, Make sure you return the bulleted text.
    * Contains the URL.
    * Multi-line string.
    """
    longtext = """
    Lorem ipsum dolor sit amet, 
    consectetur adipiscing elit, 
    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
    Ut enim ad minim veniam, 
    http://example.com
    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
    Excepteur sint occaecat cupidatat non proident, 
    sunt in culpa qui officia deserunt mollit anim id est laborum.
    """
    self.assertEqual(self._checkscript(longtext, self.FILE), "http://example.com\n")

