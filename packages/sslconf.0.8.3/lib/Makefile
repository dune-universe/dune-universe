# ---------------------------------------------------------------------------
#  Copyright (c) 2017 Tony Wuersch. All rights reserved.
#  Distributed under the ISC license, see terms at the end of the file.
#  sslconf 0.8.3 - commit 0806f5ae3774c30cc8ee18aaabe9d106d7816457
# ---------------------------------------------------------------------------

build:
	cd .. && jbuilder build @install

.PHONY: doc
doc:
	topkg publish doc

.PHONY: runtest
runtest:
	cd .. && jbuilder build @runtest --dev

.PHONY: clean
clean:
	cd .. && rm -rf _coverage doc/doc && jbuilder clean

.PHONY: coverage
coverage:
	rm -rf ../_coverage
	cd .. && jbuilder clean
	cd .. && BISECT_ENABLE=YES jbuilder runtest --dev
	cd .. && bisect-ppx-report -I _build/default/ -html _coverage/ \
	  `find . -name 'bisect*.out'`

.PHONY: distrib
distrib:
	topkg distrib

# ---------------------------------------------------------------------------
#  Copyright (c) 2017 Tony Wuersch

#  Permission to use, copy, modify, and/or distribute this software for any
#  purpose with or without fee is hereby granted, provided that the above
#  copyright notice and this permission notice appear in all copies.

#  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
#  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
#  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
#  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
#  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
#  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
#  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
# ---------------------------------------------------------------------------
