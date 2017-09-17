#
# Makfile for ascii85 encoder binary and library
#

JB 				= jbuilder
P2M_OPTS 	=	-s 1 -r "Alpha" -c "opam.ocaml.org"

all: ascii85enc.1	
		$(JB) build

clean:
		$(JB) clean

ascii85enc.1:	ascii85enc.pod Makefile
		pod2man $(P2M_OPTS) $< > $@

# OPAM - the targets below help to publish this code via opam.ocaml.org

NAME =		ascii85
VERSION =	0.4
TAG =			v$(VERSION)
GITHUB =	https://github.com/lindig/$(NAME)
ZIP =			$(GITHUB)/archive/$(TAG).zip
OPAM =		$(HOME)/Development/opam-repository/packages/$(NAME)/$(NAME).$(VERSION)

tag:
		git tag $(TAG)

descr:		README.md
		sed -n '/^# Opam/,$$ { /^#/n; p;}' $< >$@

url:
		echo	"archive: \"$(ZIP)\"" > url
		echo	"checksum: \"`curl -L $(ZIP)| md5 -q`\"" >> url

release:	url opam descr
		test -d "$(OPAM)" || mkdir -p $(OPAM)
		cp $(NAME).opam url descr $(OPAM)
