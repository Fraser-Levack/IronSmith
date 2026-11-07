#!/usr/bin/env python3
"""Migrate .irsm models to IronSmith 1.0's difference() argument order.

IronSmith 0.x: difference(a, b) meant "b minus a".
IronSmith 1.0: difference(a, b) means "a minus b" (like other CSG tools).

This swaps the two arguments of every difference(...) call, so migrated
models compile to exactly the same bytecode and render exactly as before.
It understands nesting, multi-line arguments and // comments, and keeps
each argument's surrounding whitespace in place so formatting survives.

Each migrated file gets a marker comment on its first line, and files that
already have it are skipped: running the swap twice would undo it. The
original is kept next to the file as <name>.irsm.bak.

usage: python scripts/migrate_difference.py model.irsm [more.irsm ...]
"""
import re
import shutil
import sys

MARKER = "// irsm 1.0: difference(a, b) = a minus b"

CALL = re.compile(r"(?<![A-Za-z0-9_])difference\s*\(")


def _skip_comment(s, i):
    j = s.find("\n", i)
    return len(s) if j == -1 else j


def _find_call_end(s, open_idx):
    """Index of the ')' matching s[open_idx], and the top-level commas."""
    depth, i, commas = 0, open_idx, []
    while i < len(s):
        if s.startswith("//", i):
            i = _skip_comment(s, i)
            continue
        c = s[i]
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth == 0:
                return i, commas
        elif c == "," and depth == 1:
            commas.append(i)
        i += 1
    raise ValueError("unbalanced parentheses")


def _split_ws(arg):
    core = arg.strip()
    if not core:
        return arg, "", ""
    lead = arg[: len(arg) - len(arg.lstrip())]
    trail = arg[len(arg.rstrip()):]
    return lead, core, trail


def _in_comment(s, idx):
    line_start = s.rfind("\n", 0, idx) + 1
    return "//" in s[line_start:idx]


def swap_differences(s):
    """Return `s` with the arguments of every difference(...) call swapped."""
    out, i = [], 0
    while True:
        m = next((c for c in CALL.finditer(s, i) if not _in_comment(s, c.start())), None)
        if m is None:
            out.append(s[i:])
            return "".join(out)
        open_idx = m.end() - 1
        close_idx, commas = _find_call_end(s, open_idx)
        if len(commas) != 1:
            line = s.count("\n", 0, m.start()) + 1
            raise ValueError(f"line {line}: difference() needs exactly 2 arguments")
        a = swap_differences(s[open_idx + 1: commas[0]])
        b = swap_differences(s[commas[0] + 1: close_idx])
        la, ca, ta = _split_ws(a)
        lb, cb, tb = _split_ws(b)
        out.append(s[i: open_idx + 1] + la + cb + ta + "," + lb + ca + tb + ")")
        i = close_idx + 1


def migrate(path):
    with open(path, encoding="utf-8", newline="") as f:
        text = f.read()
    if MARKER in text:
        print(f"skip  {path} (already migrated)")
        return
    count = sum(1 for c in CALL.finditer(text) if not _in_comment(text, c.start()))
    newline = "\r\n" if "\r\n" in text else "\n"
    migrated = MARKER + newline + swap_differences(text)
    shutil.copyfile(path, path + ".bak")
    with open(path, "w", encoding="utf-8", newline="") as f:
        f.write(migrated)
    print(f"ok    {path} ({count} difference call(s) swapped, original in {path}.bak)")


def main(paths):
    if not paths:
        print(__doc__.strip())
        return 2
    failed = False
    for path in paths:
        try:
            migrate(path)
        except (OSError, ValueError) as e:
            print(f"error {path}: {e}")
            failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
