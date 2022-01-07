"""
[caption]
def=Cutting URL parameters
ja=URLパラメータの切り取り
"""

import sys
import io
import tkinter
import tkinter.ttk
import tkinter.simpledialog
import ctypes
import ctypes.wintypes
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

class CuttingDialog(tkinter.simpledialog.Dialog):

  def __init__(self, parent, title, url) -> None:
    self.url = url
    super().__init__(parent, title=title)

  def body(self, master) -> None:
    parts = urlparse(self.url)
    queries = parse_qsl(parts.query)
    self.boollist = []
    self.attributes("-toolwindow", 1)
    self.attributes("-topmost", 1)
    self.focus_force()
    lf = tkinter.LabelFrame(master, text="URL")
    tkinter.Label(lf, text=f"{parts.netloc}{parts.path}").pack(pady=8, padx=4)
    lf.pack(side=tkinter.TOP)
    for query in queries:
      bv = tkinter.BooleanVar()
      tkinter.Checkbutton(master, variable=bv, text=f"{query[0]}={query[1]}").pack(side = tkinter.TOP, anchor=tkinter.W)
      self.boollist.append(bv)
    return super().body(master)

  def grab_set(self) -> None:
    p = ctypes.wintypes.POINT()
    ctypes.windll.user32.GetCursorPos(ctypes.byref(p))
    
    self.geometry(f"+{p.x - self.winfo_width() // 2}+{p.y - self.winfo_height() // 2}")
    return super().grab_set()

  def ok(self, event=None):  super().ok(event); self.result = True
  def cancel(self, event=None): super().cancel(event); self.result = False
  
text = sys.stdin.read()

if text != "":
  result = True
  bools = []
  p = urlparse(text)  
  if "params" in globals():
    bools = globals()["params"]["bools"]
  elif p.scheme:
    owner = tkinter.Tk()
    owner.withdraw()
    dlg = CuttingDialog(owner, 'Cutting URL Params', text)
    bools = dlg.boollist
    result = dlg.result

  if result:
    url = urlparse(text)
    qsls = parse_qsl(url.query)
    qsla = []
    for b, q in zip(bools, qsls):
      if b.get() if type(b) is tkinter.BooleanVar else b:
        qsla.append((q[0], q[1]))
    print(urlunparse(url._replace(query=urlencode(qsla))))
  else:
    print(text)
