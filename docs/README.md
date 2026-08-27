# ILEARN × WIDA ACCESS — presentation

Reveal.js (Quarto) deck of the Indiana ILEARN ELA × WIDA ACCESS Overall
signal-vs-noise analysis. Renders to **`docs/index.html`** so GitHub Pages
can serve it: **Settings → Pages → Deploy from a branch → `/docs`**.

The deck never reads student-level files. It consumes
`outputs/manifests/results.json` (aggregates) and the PNG figures under
`figures/`.

## Present it

Open **`docs/index.html`** in any browser (self-contained, works offline).

- **`S`** — speaker view (per-slide notes + timer)
- **`F`** — fullscreen · **`O`** — overview · arrows / space to advance
- Click any figure to enlarge it

## Build

From the analysis root:

```bash
Rscript run_all.R          # refresh aggregates + JSON manifests
./docs/render.sh           # render this deck
```

or `make -C docs`.

Requires [Quarto](https://quarto.org) ≥ 1.4 and R with `data.table`,
`jsonlite`.

## Template

Theme, figure lightbox, and slide vocabulary follow
`DBetebenner/Rhode_Island_082726_Presentation` (Josefin / Noto, 1920×1080
reveal.js, `embed-resources: true`).
