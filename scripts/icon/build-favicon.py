"""Packs rendered PNGs into a multi-size favicon.ico.

The small sizes come from the simplified mark and the large one from the full mark: an icon file
may carry different artwork per size, and at sixteen pixels the tally marks are a smudge over the
only shape that could still have read.
"""
import struct
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).parent
SERVED = HERE / ".." / ".." / "backend" / "src" / "main" / "resources" / "web"
SOURCES = [("icon-small.svg.png", 16), ("icon-small.svg.png", 32),
           ("icon-small.svg.png", 48), ("icon.svg.png", 128)]


def main() -> int:
    blobs = []
    for source, size in SOURCES:
        if not (HERE / source).is_file():
            print(f"{source} is missing; render it with qlmanage first (see README.md)")
            return 1
        out = HERE / f"ico{size}.png"
        subprocess.run(["sips", "-z", str(size), str(size), str(HERE / source), "--out", str(out)],
                       check=True, capture_output=True)
        blobs.append((out.read_bytes(), size))
        out.unlink()

    offset = 6 + 16 * len(blobs)
    entries, body = b"", b""
    for data, size in blobs:
        side = size if size < 256 else 0
        entries += struct.pack("<BBBBHHII", side, side, 0, 0, 1, 32, len(data), offset)
        body += data
        offset += len(data)
    written = struct.pack("<HHH", 0, 1, len(blobs)) + entries + body
    (SERVED / "favicon.ico").resolve().write_bytes(written)
    print(f"favicon.ico: {len(blobs)} sizes, {len(written)} bytes")
    return 0


if __name__ == "__main__":
    sys.exit(main())
