# ClipboardMonitor Extended Manual

This application stores Python scripts in `%LOCALAPPDATA%\ClipboardMonitor`.

This folder can also be opened from the application context menu, so add or edit scripts as needed.

Here is a summary of what you can do with this script and how to input and output.

In addition, Files whose file names begin with an underscore(_) are reserved for the ClipboardMonitor system.

When adding a script in the future, **an underscore will always be added at the beginning of the file name**, so if you create your own script, **please give it a name that does not start with the underscore**.

## Matters common to all

The items common to both the copy time script and the conversion script are as follows.

* The script is executed with the character string stored in the clipboard written to the standard input.
* The character string written to the standard output is taken as the output of the script.
* If an error occurs, the contents of the standard error output can be referenced from the application.
* Standard input, standard output, and standard error output are all treated as UTF-8 encoded text.
* You can create a setting area at the top of the script file that defines the display name and display order of the script (optional).

### What is the setting area?

A value that defines how the script will appear on the application's menu and the order in which it will be sorted on the menu.
It is in the form of an INI file, and is basically enclosed in three double quotes before and after (to prevent execution on Python).
The sections and keys supported as of v1.0 are as follows.

* caption section: Specifies the name displayed on the menu. If not exists, the file name will be displayed.
  * def: The name displayed when the language setting is def (English)
  * ja: The name displayed when the language setting is ja (Japanese)
* general section
  * order: The order on the menu of the script. The one closer to 0 is ranked higher. If not specified, it is considered that 100 is set.

#### Example sentence (from _charcount.py)

```python
"""
[caption]
def = & Counting characters
ja = Count of characters (& C)
[general]
order = 100
"""

```

When a blank line is entered, the definition of the Ini file ends there.

## Copy script (Run folder)

The copy-time script is called when text is added to the clipboard.

What you write to standard output is displayed in the bottom status view of your application.

A very simple HTML tag can be used here. The tags that can be used are as follows.

* p
* a
* b
* i
* pre
* ul
* ol
* li
* dl
* dt
* dd
* br
* span
* div

Only simple CSS styles such as color specification can be used.

### Script description example (from _colorcode.py)

```python
import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()
m =  re.match(r"^#([0-9a-f]{6}|[0-9a-f]{3})$", text, re.IGNORECASE)
if m: print(f'<p>Color:<span style="color:#{m[1]};">#{m[1]}</span></p>')
```

## Conversion script

The contents of the clipboard can be rewritten by selecting the conversion script from the conversion script menu.

The content written to the standard output is pasted to the clipboard (if there is a newline at the end, it is deleted).

### Script description example (from _bullet.py)

```python
import sys, io, re

sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

text = sys.stdin.read()

if text != "":
  print(re.sub(r"^.+$", "* \g<0>", text, flags=re.M))
```
