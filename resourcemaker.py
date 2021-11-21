from argparse import ArgumentParser
from pathlib import Path
import subprocess

p = ArgumentParser()
p.add_argument("--lazarus_dir", required=True)
args = p.parse_args()

lazres = Path(args.lazarus_dir) / "tools" / "lazres"

cmds = ["scripts.lrs"]

for f in Path(__file__).parent.glob("**/_*.py"):
  if f.parent.name != "backup":
    cmds.append(f"{f}={f.parent.name[0]}{f.name[1:]}")

cmdline = f"{str(lazres)} {' '.join(cmds)}"
print(f"> {cmdline}")
print(subprocess.check_output(cmdline).decode("UTF-8"))
