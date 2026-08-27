# ILEARN × WIDA ACCESS deck
#
#   make            # render docs/index.html
#   make clean      # remove rendered HTML + Quarto scratch

QMD  := IN_ILEARN_WIDA_ACCESS.qmd
HTML := index.html

.PHONY: all render clean

all: render

render:
	./render.sh

clean:
	rm -f $(HTML)
	rm -rf .quarto IN_ILEARN_WIDA_ACCESS_files IN_ILEARN_WIDA_ACCESS_cache
