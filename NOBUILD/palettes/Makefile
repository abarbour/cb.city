all: FORCE

P=cb.palettes
PAL=$(P).txt
RDA=$(P).rda

FORCE: $(RDA) clean

$(PAL): 01.pal_html
	$< > $@

$(RDA): to.R $(PAL)
	Rscript --no-save $<

clean:
	rm -f tmp.*
