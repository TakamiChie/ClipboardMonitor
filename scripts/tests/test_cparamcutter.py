from baseclass import BaseClass

from urllib.parse import urlparse, urlunparse, parse_qsl

class TestCParamCutter(BaseClass):
  
  FILE = "conversion/_paramcutter"
  BASEURL = "https://www.amazon.co.jp/%25E9%25AB%2598%25E8%25A6%258B-%25E7%259F%25A5%25E8%258B%25B1/e/B07RQLKZFJ?ref=sr_ntt_srch_lnk_1&qid=1641537879&sr=8-1"
  NOQUERY = urlunparse(urlparse(BASEURL)._replace(query=""))
 
  def test_blank(self):
    """
    If you pass a string with the following conditions to _paramcutter.py, Make sure that nothing is output.
    * No character string specified.
    """
    self.assertEqual(self._checkscript("", self.FILE), "")
 
  def test_nolink(self):
    """
    If you pass a string with the following conditions to _paramcutter.py, Make sure that you do not display the dialog.
    * String is not a URL.
    """
    self.assertEqual(self._checkscript("test", self.FILE), "test\n")

  def test_url_no_query(self):
    """
    If you pass a string with the following conditions to _paramcutter.py, Confirm that the URL is returned as it is.
    * URLs that do not contain a query.
    """
    param = {
      "params": {
        "bools": []
      }
    }
    self.assertEqual(self._checkscript(self.NOQUERY, self.FILE, parameters=param), f"{self.NOQUERY}\n")

  def test_url_removeall(self):
    """
    If you pass a string with the following conditions to _paramcutter.py, Make sure that the specified location of the URL query is deleted.
    * The URL has a query string.
    * Specify to delete all queries.
    """
    p = urlparse(self.BASEURL)
    param = {
      "params": {
        "bools": [False] * len(parse_qsl(p.query))
      }
    }
    self.assertEqual(self._checkscript(self.BASEURL, self.FILE, parameters=param), f"{self.NOQUERY}\n")

  def test_url_removepart(self):
    """
    If you pass a string with the following conditions to _paramcutter.py, Make sure that the specified location of the URL query is deleted.
    * The URL has a query string.
    * Specify that only one query is deleted.
    """
    p = urlparse(self.BASEURL)
    q = parse_qsl(p.query)
    bools = [False] * len(q)
    bools[2] = True
    param = {
      "params": {
        "bools": bools
      }
    }
    self.assertEqual(self._checkscript(self.BASEURL, self.FILE, parameters=param), f"{self.NOQUERY}?{q[2][0]}={q[2][1]}\n")
