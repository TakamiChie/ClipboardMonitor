from baseclass import BaseClass

class TestURLChecker(BaseClass):
  
  FILE = "run/_urlchecker"

  def test_nourl(self):
    """
    If a character string is specified under the following conditions, check if the result is not output.
    * String without URL
    """
    self.assertEqual(self._checkscript("test", self.FILE), "")

  def test_httpurl(self):
    """
    If a character string is specified under the following conditions, Confirm that you want to return the page title and status code.
    * HTTP URL
    """
    self.assertListEqual(self._checkscript("http://example.com", self.FILE).split('\n'), 
      ['<p>Example Domain</p>', '<p>200 OK</p>', ''])

  def test_httpsurl(self):
    """
    If a character string is specified under the following conditions, Confirm that you want to return the page title and status code.
    * HTTPS URL
    """
    self.assertListEqual(self._checkscript("https://example.com/", self.FILE).split('\n'), 
      ['<p>Example Domain</p>', "<p>200 OK</p>", ''])

  def test_nowebsite(self):
    """
    If a character string is specified under the following conditions, Confirm that you want to return the status code.
    * URL pointing to an image resource
    """
    self.assertListEqual(self._checkscript("https://www.iana.org/_img/2015.1/iana-logo-homepage.svg", self.FILE).split('\n'), 
      ["<p>200 OK</p>", ''])

  def test_httpsurl_noexists(self):
    """
    If a character string is specified under the following conditions, Confirm that you want to return the page title and status code.
    * A page address that does not exist.
    """
    self.assertListEqual(self._checkscript("https://example.com/notfound", self.FILE).split('\n'), 
      ["<p style='color: red'>404 Not Found</p>", ''])
