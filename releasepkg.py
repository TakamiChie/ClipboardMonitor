## Search Lazarus Root
import argparse
import winreg
from pathlib import Path
import shutil
import xml.etree.ElementTree as ElementTree
import subprocess
import datetime
import zipfile
from argparse import ArgumentParser

APPLICATION_NAME = "CMon"
APPLICATION_EXENAME = f"{APPLICATION_NAME}.exe"
APPLICTAION_LPIFILE = f"{APPLICATION_NAME}.lpi"

lazarus_root = None
argp = ArgumentParser()
argp.add_argument("-t", default=False, action="store_true", 
  help="Enter script test mode. Do not update the build number of the lpi file in test mode.")
argv = argp.parse_args()

print(">> Clean up")
rootdir = Path(__file__).parent
releasedir = rootdir / "Release"
if releasedir.exists(): shutil.rmtree(releasedir)
releasedir.mkdir()

print(">> Find Lazarus")
with winreg.OpenKeyEx(winreg.HKEY_LOCAL_MACHINE, r'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\lazarus_is1',
  access=winreg.KEY_READ | winreg.KEY_WOW64_64KEY) as k:
  lazarus_root, _ = winreg.QueryValueEx(k, "InstallLocation")

if lazarus_root is None: raise Exception("Lazarus Not Found!")
print(f"Found! {lazarus_root}")

## Get Version
print(">> Get info")
tree = ElementTree.parse(APPLICTAION_LPIFILE)
vi = tree.find("ProjectOptions").find("VersionInfo")
ver = lambda n: 0 if n is None else int(n.get('Value')) 
vn = f"{ver(vi.find('MajorVersionNr'))}.{ver(vi.find('MinorVersionNr'))}.{ver(vi.find('RevisionNr'))}.{ver(vi.find('BuildNr'))}"
if argv.t:
  print("Since it is in test mode, the build number is not updated.")
else:
  buildnr = vi.find("BuildNr")
  if buildnr is None: buildnr = ElementTree.SubElement(vi, "BuildNr")
  newbn = int(datetime.datetime.now().strftime("%y%m%d")[1:]) // 2
  buildnr.set("Value", str(newbn))
  tree.write(APPLICTAION_LPIFILE, encoding="UTF-8", xml_declaration=True)
  # Repair format
  with open(APPLICTAION_LPIFILE, "r") as f:
    lines = f.read().splitlines()
  with open(APPLICTAION_LPIFILE, "w") as f:
    for line in lines:
      if line.startswith("<?xml"): f.write(line.replace("'", '"'))
      elif " />" in line: f.write(line.replace(" />", "/>"))
      else: f.write(line)
      f.write("\n")
  print((f"Version:{vn}"))

ch = subprocess.check_output("git log -n 1 --pretty=format:%h".split(" ")).decode("UTF-8")
print((f"CommitHash:{ch}"))

print(">> Create Release Package")
proc = subprocess.Popen(f"{lazarus_root}\\lazbuild --build-mode=Release --no-write-project {APPLICTAION_LPIFILE}", shell=True);
proc.communicate()

print(">> Create HTML Readme")
for f in rootdir.iterdir():
  if f.is_file() and f.name.endswith(".md"):
    print(f"> {f.name}")
    subprocess.check_call(f"pandoc -f markdown -t html -o Release/{f.stem}.html {f.name}".split(" "))


fn = rootdir / f"{APPLICATION_NAME}-{vn}-{ch}.zip"
print(f">> Create {APPLICATION_NAME}-{vn}-{ch}.zip")
with zipfile.ZipFile(fn, "w", 
  compression=zipfile.ZIP_DEFLATED) as zfile:
  for f in releasedir.iterdir():
    print(f"> {f.name}")
    zfile.write(f"Release/{f.name}", arcname=f"{f.name}")
  for f in rootdir.iterdir():
    if f.is_file():
      if f.name in ["LICENSE"] or f.name.endswith(".md"):
        print(f"> {f.name}")
        zfile.write(f.name)
    if f.is_dir():
      if f.name.endswith(".assets"):
        print(f"> {f.name}")
        zfile.write(f.name)
        for d in f.iterdir():
          if d.is_file():
            zfile.write(f"{f.name}/{d.name}")

print(f"Release Package Create Complete! at '{fn}")