from baseclass import BaseClass
import os
import subprocess

class TestCChangeLinkFormatURL(BaseClass):
  
  FILE = "conversion/_changelinkformat"
  
  def test_blank(self):
    """
    If you pass a string with the following conditions to _leaveurl.py, Make sure that nothing is output.
    * No character string specified.
    """
    param = {
      "params": {
        "source": "Plain Text",
        "destination": "HTML"
      }
    }
    self.assertEqual(self._checkscript("", self.FILE), "")

  def test_plaintext2HTML(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains plain text URLs.
    * Source link format: Plain Text
    * Destination link format: HTML
    """
    pretext = "plaintext link Example Domain - http://example.com/ abcde"
    expectedtext = '<a href="http://example.com/">plaintext link Example Domain</a> abcde\n'
    param = {
      "params": {
        "source": "Plain Text",
        "destination": "HTML"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_plaintext2Markdown(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains plain text URLs.
    * Source link format: Plain Text
    * Destination link format: Markdown
    """
    pretext = "plaintext link Example Domain - http://example.com/ abcde"
    expectedtext = '[plaintext link Example Domain](http://example.com/) abcde\n'
    param = {
      "params": {
        "source": "Plain Text",
        "destination": "Markdown"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_plaintext2Mediawiki(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains plain text URLs.
    * Source link format: Plain Text
    * Destination link format: MediaWiki
    """
    pretext = "plaintext link Example Domain - http://example.com/ abcde"
    expectedtext = '[http://example.com/ plaintext link Example Domain] abcde\n'
    param = {
      "params": {
        "source": "Plain Text",
        "destination": "MediaWiki"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_plaintext2Plaintextplus(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains plain text URLs.
    * Source link format: Plain Text
    * Destination link format: Plain Text+
    """
    pretext = "plaintext link Example Domain - http://example.com/ abcde"
    expectedtext = 'plaintext link Example Domain [http://example.com/](http://example.com/) abcde\n'
    param = {
      "params": {
        "source": "Plain Text",
        "destination": "PlainText+"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_HTML2Plaintext(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains HTML URLs.
    * Source link format: HTML
    * Destination link format: Plain Text
    """
    pretext = 'plaintext link <a href="http://example.com/">Example Domain</a> abcde'
    expectedtext = 'plaintext link Example Domain - http://example.com/ abcde\n'
    param = {
      "params": {
        "source": "HTML",
        "destination": "Plain Text"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_Markdown2Plaintext(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains Markdown URLs.
    * Source link format: Markdown
    * Destination link format: Plain Text
    """
    pretext = 'plaintext link [Example Domain](http://example.com/) abcde'
    expectedtext = 'plaintext link Example Domain - http://example.com/ abcde\n'
    param = {
      "params": {
        "source": "Markdown",
        "destination": "Plain Text"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_Mediawiki2Plaintext(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains MediaWiki URLs.
    * Source link format: MediaWiki
    * Destination link format: Plain Text
    """
    pretext = 'plaintext link [http://example.com/ Example Domain] abcde'
    expectedtext = 'plaintext link Example Domain - http://example.com/ abcde\n'
    param = {
      "params": {
        "source": "MediaWiki",
        "destination": "Plain Text"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)

  def test_Plaintextplus2Plaintext(self):
    """
    If you pass a string with the following conditions to _bullet.py, Confirm that the URL format is changed.
    * Contains PlainText+ URLs.
    * Source link format: PlainText+
    * Destination link format: Plain Text
    """
    pretext = 'plaintext link Example Domain [http://example.com/](http://example.com/) abcde'
    expectedtext = 'plaintext link Example Domain - http://example.com/ abcde\n'
    param = {
      "params": {
        "source": "PlainText+",
        "destination": "Plain Text"
      }
    }
    self.assertEqual(self._checkscript(pretext, self.FILE, parameters=param), expectedtext)
