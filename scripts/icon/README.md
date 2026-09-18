# The app's icon

`icon.svg` is the mark: a flexed arm curling a dumbbell, with five tally marks in the top-left
corner — counting, without a word in it, so it needs no translating.

`icon-small.svg` is the same mark with the tally and the fine detail taken out and the arm scaled
to fill the frame. A favicon is sixteen pixels across; everything that is a nice touch at 512 is a
grey smudge there, sitting on top of the one shape that could still have read.

`icon-maskable.svg` is the full mark shrunk into the *safe zone* — a circle of radius 40% of the
width, centred — with the background still reaching every edge. Android launchers crop an icon
declared maskable to their own shape (a circle on a Pixel, a squircle on a Samsung), so anything
outside that circle may be cut. Without a maskable icon at all, Android shrinks the plain one and
sets it on a white disk instead. The scale is worked out from the farthest drawn point, the
tally's corner.

## Regenerating the served files

macOS only, using what the system already has — no image toolchain to install:

```bash
cd scripts/icon
qlmanage -t -s 512 -o . icon.svg && qlmanage -t -s 512 -o . icon-small.svg
sips -z 192 192 icon.svg.png --out ../../backend/src/main/resources/web/icon-192.png
cp icon.svg.png ../../backend/src/main/resources/web/icon-512.png
qlmanage -t -s 512 -o . icon-maskable.svg
cp icon-maskable.svg.png ../../backend/src/main/resources/web/icon-maskable-512.png
sips -z 192 192 icon-maskable.svg.png --out ../../backend/src/main/resources/web/icon-maskable-192.png
```

An XML comment may not contain two hyphens in a row. `qlmanage` does not refuse such a file: it
renders the parser's error page instead, and that page is what ends up in the PNG. Check each
source parses before rendering it.

`favicon.ico` carries four sizes — 16, 32 and 48 from the small mark, 128 from the full one — as
PNGs inside an ICO container, which every browser in use reads. `build-favicon.py` assembles it.
