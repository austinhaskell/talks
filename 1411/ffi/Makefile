PANDOC ?= pandoc
THEME  ?= sky

.PHONY: all
all : ffi.html

ffi.html : ffi.markdown
	$(PANDOC) -t revealjs -V theme=$(THEME) -s -o $@ $^

demo:
	ghci cbits/demo.o Demo.hs

.PHONY: clean
clean :
	-rm ffi.html
