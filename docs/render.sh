#!/usr/bin/env bash
#
# Render the ILEARN × WIDA ACCESS deck to docs/index.html.
# Reads outputs/manifests/results.json — never student-level cache.
#
#   ./docs/render.sh
#   make -C docs
#
set -euo pipefail

cd "$(dirname "$0")"

if [[ ! -f ../outputs/manifests/results.json ]]; then
  echo "Missing ../outputs/manifests/results.json" >&2
  echo "Run: Rscript run_all.R 8" >&2
  exit 1
fi

echo "==> Rendering docs/IN_ILEARN_WIDA_ACCESS.qmd"
quarto render IN_ILEARN_WIDA_ACCESS.qmd --to revealjs
rm -rf IN_ILEARN_WIDA_ACCESS_files IN_ILEARN_WIDA_ACCESS_cache .quarto

echo "==> Done: docs/index.html (GitHub Pages: serve from /docs)"
