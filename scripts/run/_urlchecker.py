"""
[caption]
def=URL &resolution
ja=URLの解決(&R)
[general]
order=0
"""

import urllib.request, sys, ssl, io
from html.parser import HTMLParser

class TitleParser(HTMLParser):
  def feed(self, data: str):
    self.tag = ""
    self.title = ""
    return super().feed(data)

  def handle_starttag(self, tag: str, attrs: list):
    self.tag = tag
    return super().handle_starttag(tag, attrs)

  def handle_data(self, data: str) -> None:
    if self.tag == "title" and self.title == "":
      self.title = data
    return super().handle_data(data)

sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

url = sys.stdin.read()

if url.startswith("http://") or url.startswith("https://"):
  try:
    ssl._create_default_https_context = ssl._create_unverified_context
    req = urllib.request.Request(url, headers={'User-Agent': "Mozilla/5.0"}) 
    with urllib.request.urlopen(req) as responce:
      ct = responce.info()["Content-Type"]
      if "text/html" in ct:
        charset = ct.split("charset=")[-1]if "charset" in ct else "UTF-8"
        t = TitleParser()
        t.feed(responce.read().decode(charset))
        if t.title != "": print(f"<p>{t.title}</p>")
      print(f"<p>{responce.status} {responce.reason}</p>")
  except urllib.error.HTTPError as err:
    print(f"<p style='color: red'>{err.code} {err.reason}</p>")
  except urllib.error.URLError as err:
    print(f"<p style='color: red'>{err.reason}</p>")
