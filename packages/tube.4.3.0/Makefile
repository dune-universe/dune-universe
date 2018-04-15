.PHONY: clean build utop async lwt

all: build

clean:
	-rm -rf _build
	-rm -rf src/.merlin
	-rm -rf *.install

build:
	jbuilder build --only-packages=tube @install

utop:
	jbuilder exec utop
