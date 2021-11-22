"""
[caption]
def=&Change the link format
ja=リンク形式の変更(&C)
"""

import sys, io, re, tkinter, tkinter.ttk, tkinter.simpledialog, ctypes, ctypes.wintypes

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

LINK_FORMATS = ["Plain Text", "HTML", "Markdown", "MediaWiki", "PlainText+"]
LINK_PATTERNS = [
  r"(?P<text>.*)\s(?=http)(?P<url>https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)",
  r"<a[^>]*href=\"(?P<url>https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)\"[^>]*>(?P<text>.*)</a>",
  r"\[(?P<text>.*?)\]\((?P<url>https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)\)",
  r"\[(?P<url>https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)\s(?P<text>.*?)\]",
  r"(?P<text>.*?)\s\[(?P<url>https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)\]\((?:https?://[\w/:%#\$&\?\(\)~\.=\+\-]+)\)"
]
LINK_TEXT = [
  "{text} - {url}",
  '<a href="{url}">{text}</a>',
  '[{text}]({url})',
  '[{url} {text}]',
  '{text} [{url}]({url})'
]

class ChooseDialog(tkinter.simpledialog.Dialog):

  def body(self, master) -> None:
    self.fromvar = tkinter.StringVar(master)
    self.tovar = tkinter.StringVar(master)
    self.attributes("-toolwindow", 1)
    self.attributes("-topmost", 1)
    self.focus_force()
    for cb in [("from", self.fromvar), ("to", self.tovar)]:
      lf = tkinter.LabelFrame(master, text=cb[0])
      cbx = tkinter.ttk.Combobox(lf, state='readonly', values=LINK_FORMATS, textvariable=cb[1])
      cbx.set(LINK_FORMATS[(0 if cb[0] == "from" else 1)])
      cbx.pack(padx=8, pady=4)
      lf.pack(side=tkinter.TOP)
    return super().body(master)

  def grab_set(self) -> None:
    p = ctypes.wintypes.POINT()
    ctypes.windll.user32.GetCursorPos(ctypes.byref(p))
    
    self.geometry(f"+{p.x - self.winfo_width() // 2}+{p.y - self.winfo_height() // 2}")
    return super().grab_set()

  def ok(self, event=None):  super().ok(event); self.result = True
  def cancel(self, event=None): super().cancel(event); self.result = False
  
if not sys.stdin.isatty() or len(sys.argv) == 2:
  text = sys.argv[-1] if len(sys.argv) == 2 else sys.stdin.read()

  owner = tkinter.Tk()
  owner.withdraw()
  dlg = ChooseDialog(owner, 'Choose Link Style')

  fromrexp = ''
  toformat = ''
  fromrexp = LINK_PATTERNS[LINK_FORMATS.index(dlg.fromvar.get())]
  toformat = LINK_TEXT[LINK_FORMATS.index(dlg.tovar.get())]

  if dlg.result:
    print(re.sub(fromrexp, lambda m: toformat.format(url=m["url"], text=m["text"].replace('\\', '')), text))
  else:
    print(text)

