import urllib.request, sys, ssl

if not sys.stdin.isatty():
  url = input().lower()

  if url.startswith("http://") or url.startswith("https://"):
    try:
      ssl._create_default_https_context = ssl._create_unverified_context
      with urllib.request.urlopen(url) as responce:
        print(f"{responce.status} {responce.reason}")
    except urllib.error.HTTPError as err:
      print(f"{err.code} {err.reason}")
    except urllib.error.URLError as err:
      print(f"{err.reason}")
