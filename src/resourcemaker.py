from argparse import ArgumentParser
from pathlib import Path
import subprocess
import datetime

p = ArgumentParser()
p.add_argument("--lazarus_dir", required=True)
args = p.parse_args()

### Create lrs file.

lazres = Path(args.lazarus_dir) / "tools" / "lazres"

cmds = ["scripts.lrs"]

for f in Path(__file__).parent.glob("**/_*.py"):
  if f.parent.name in ["conversion", "run"]:
    cmds.append(f"{f}={f.parent.name[0]}{f.name[1:]}")

cmdline = f"{str(lazres)} {' '.join(cmds)}"
print(f"> {cmdline}")
print(subprocess.check_output(cmdline).decode("UTF-8"))

### Add Version Information

date = datetime.datetime.now().strftime('%Y/%m/%d %H:%M:%S')
parentdir = Path(__file__).parent.parent
chash = subprocess.check_output("git log -n 1 --pretty=format:commit_hash=%h".split(" "), cwd=str(parentdir)).decode("UTF-8")
cdate = subprocess.check_output("git log -n 1 --pretty=format:commit_date=%cd --date=format:%Y/%m/%d".split(" "), cwd=str(parentdir)).decode("UTF-8")
ini = f"""
[data]
build_date={date}
{chash}
{cdate}
"""
print(ini);
with open(parentdir / "buildinfo.ini", 'w') as f:
  f.write(ini)


