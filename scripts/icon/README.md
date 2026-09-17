# The app's icon

`icon.svg` is the mark: a flexed arm curling a dumbbell, with five tally marks in the top-left
corner — counting, without a word in it, so it needs no translating.

`icon-small.svg` is the same mark with the tally and the fine detail taken out and the arm scaled
to fill the frame. A favicon is sixteen pixels across; everything that is a nice touch at 512 is a
grey smudge there, sitting on top of the one shape that could still have read.

## Regenerating the served files

macOS only, using what the system already has — no image toolchain to install:

```bash
cd scripts/icon
qlmanage -t -s 512 -o . icon.svg && qlmanage -t -s 512 -o . icon-small.svg
sips -z 192 192 icon.svg.png --out ../../backend/src/main/resources/web/icon-192.png
cp icon.svg.png ../../backend/src/main/resources/web/icon-512.png
```

`favicon.ico` carries four sizes — 16, 32 and 48 from the small mark, 128 from the full one — as
PNGs inside an ICO container, which every browser in use reads. `build-favicon.py` assembles it.
