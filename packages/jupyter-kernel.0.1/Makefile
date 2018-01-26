
all: build

build:
	jbuilder build @install

clean:
	jbuilder clean

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make all; \
	done
