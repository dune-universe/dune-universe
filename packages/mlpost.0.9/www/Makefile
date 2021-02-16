
WWW = /users/www-perso/projets/mlpost/

GENERATED := index.fr.html  index.en.html index.de.html \
	   manual.fr.html manual.en.html manual.de.html

ALL:= $(GENERATED) slides-jfla.pdf mlpost-jfla.pdf

install: all
	cp -f --parents $(ALL) $(WWW)

export: all
	scp $(ALL) filliatr@pl-ssh2.lri.fr:/users/www-perso/projets/mlpost/

all: $(ALL)

$(GENERATED): macros.prehtml version.prehtml

# RULES

YAMLPP=yamlpp macros.prehtml version.prehtml

.SUFFIXES: .html .prehtml .fr.html .en.html

%.fr.html: %.prehtml
	$(YAMLPP) -l fr $< -o $@

%.en.html: %.prehtml
	$(YAMLPP) -l en $< -o $@

%.de.html: %.prehtml
	$(YAMLPP) -l de $< -o $@

clean:
	rm -f $(GENERATED)
