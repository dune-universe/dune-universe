.PHONY: lib all clean test doc

BUILD_LIB=dune build	
CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
DOC=dune build @doc
INSTALL=dune install

lib:
	${BUILD_LIB}

all:
	${BUILD_LIB}

test:
	${TEST}

doc:
	${DOC}

install:
	${INSTALL}

clean:
	${CLEAN}
