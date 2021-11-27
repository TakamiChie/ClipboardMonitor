from argparse import ArgumentParser
import sys

args = ArgumentParser()
args.add_argument("--raiseerror", action='store_true')
argv = args.parse_args()
print("Hello This Is a TEST");
if (s := sys.stdin.read()) != "\n": print(s, end="")
if argv.raiseerror: raise Exception("This Is Test Error")
