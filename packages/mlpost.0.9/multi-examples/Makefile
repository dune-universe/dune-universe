MLFILES := $(shell echo *.ml)
MLFILES := $(MLFILES:myocamlbuild.ml=)
MLFILES := $(MLFILES:examples.ml=)

BUILD = _build/

OCAMLBUILD := ocamlbuild
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif
ifneq ($(BUILD), _build/)
	OCAMLBUILD := $(OCAMLBUILD) -build-dir $(BUILD)
endif

HTML = $(BUILD)examples.html

all:
	$(OCAMLBUILD) $(MLFILES:.ml=.htmlpart)
	echo "<html><head><title>Mlpost Examples</title></head>" > $(HTML)
	echo "<body><h1>Mlpost Examples</h1>" >> $(HTML)
	cat $(BUILD)*.htmlpart >> $(HTML)
	echo "</body></html>" >> $(HTML)

clean:
	$(OCAMLBUILD) -clean
	rm -f *~

.PHONY: all