from baseclass import BaseClass
import os
import subprocess

class TestCCleanAmazonLink(BaseClass):
  
  FILE = "conversion/_cleanamazonlink"
  ANSWERURL = "https://www.amazon.co.jp/dp/4798062855/\n"
  
  def test_blank(self):
    """
    If you pass a string with the following conditions to _cleanamazonlink.py, Make sure that nothing is output.
    * No character string specified.
    """
    self.assertEqual(self._checkscript("", self.FILE), "")

  def test_nowork(self):
    """
    When setting a URL that is not a URL of Amazon is set to _cleanamazonlink.py, it is confirmed that no processing is processed.
    """
    TESTURL = "https://sbc.yokohama/"
    self.assertEqual(self._checkscript(TESTURL, self.FILE), f"{TESTURL}\n")

  def test_nothing(self):
    """
    If the URL satisfying the following conditions is specified as _cleanamazonlink.py, it is confirmed that the URL to which the extra URL portion has been deleted is acquired.
    * Product name: No
    * Suffix: No
    * URL Last Slash: Yes
    """
    TESTURL = self.ANSWERURL[:-1]
    self.assertEqual(self._checkscript(TESTURL, self.FILE), self.ANSWERURL)

  def test_fullurl2clean(self):
    """
    If the URL satisfying the following conditions is specified as _cleanamazonlink.py, it is confirmed that the URL to which the extra URL portion has been deleted is acquired.
    * Product name: Yes
    * Suffix: Yes
    * URL Last Slash: Yes
    """
    TESTURL = "https://www.amazon.co.jp/%E3%81%AF%E3%81%98%E3%82%81%E3%81%A6%E3%81%AEMicrosoft-365-Teams-BASIC-MASTER/dp/4798062855/ref=sr_1_1?__mk_ja_JP=%E3%82%AB%E3%82%BF%E3%82%AB%E3%83%8A&crid=18W29S0PL77I3&keywords=%E9%AB%98%E8%A6%8B%E7%9F%A5%E8%8B%B1&qid=1641223155&sprefix=%2Caps%2C445&sr=8-1"
    self.assertEqual(self._checkscript(TESTURL, self.FILE), self.ANSWERURL)

  def test_productname2clean(self):
    """
    If the URL satisfying the following conditions is specified as _cleanamazonlink.py, it is confirmed that the URL to which the extra URL portion has been deleted is acquired.
    * Product name: Yes
    * Suffix: No
    * URL Last Slash: Yes
    """
    TESTURL = "https://www.amazon.co.jp/%E3%81%AF%E3%81%98%E3%82%81%E3%81%A6%E3%81%AEMicrosoft-365-Teams-BASIC-MASTER/dp/4798062855/"
    self.assertEqual(self._checkscript(TESTURL, self.FILE), self.ANSWERURL)

  def test_suffix2clean(self):
    """
    If the URL satisfying the following conditions is specified as _cleanamazonlink.py, it is confirmed that the URL to which the extra URL portion has been deleted is acquired.
    * Product name: No
    * Suffix: Yes
    * URL Last Slash: Yes
    """
    TESTURL = "https://www.amazon.co.jp/dp/4798062855/ref=sr_1_1?__mk_ja_JP=%E3%82%AB%E3%82%BF%E3%82%AB%E3%83%8A&crid=18W29S0PL77I3&keywords=%E9%AB%98%E8%A6%8B%E7%9F%A5%E8%8B%B1&qid=1641223155&sprefix=%2Caps%2C445&sr=8-1"
    self.assertEqual(self._checkscript(TESTURL, self.FILE), self.ANSWERURL)

  def test_noslash2clean(self):
    """
    If the URL satisfying the following conditions is specified as _cleanamazonlink.py, it is confirmed that the URL to which the extra URL portion has been deleted is acquired.
    * Product name: No
    * Suffix: No
    * URL Last Slash: No
    """
    TESTURL = self.ANSWERURL[:-2]
    self.assertEqual(self._checkscript(TESTURL, self.FILE), f"{TESTURL}\n")

