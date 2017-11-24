# This file is part of the Watson Conversation Service OCaml API project.
#
# Copyright 2016-2017 IBM Corporation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

WCSLIB=`ocamlfind query wcs-lib`
WCSAPI=`ocamlfind query wcs-api`

OCAMLBUILD= ocamlbuild \
		-no-links -classic-display \
		-tags annot,debug,bin_annot

RMLBUILD= rmlbuild \
		-lflags "-I","`rmlc -where`","rmllib.cmxa" \
		-no-links -classic-display \
		-tags debug,annot,thread \
		-lflags "-thread"

TARGET=native
RMAIN=r_rulebot

GENERATED= bmd/io_t.ml \
	   bmd/io_t.mli \
	   bmd/io_j.ml \
	   bmd/io_j.mli \
	   call_t.ml \
	   call_t.mli \
	   call_j.ml \
	   call_j.mli \
	   dialog_interface_t.ml \
	   dialog_interface_t.mli \
	   dialog_interface_j.ml \
	   dialog_interface_j.mli

all: $(RMAIN)

.PHONY: $(RMAIN).$(TARGET) clean cleanall

$(RMAIN): $(RMAIN).$(TARGET)
	cp _build/$< $@

$(RMAIN).$(TARGET): $(GENERATED)
	$(RMLBUILD) \
		-rmlc "rmlc -I $(WCSLIB) -I $(WCSAPI)" \
		-I api -I bmd -I parser -I workspaces \
		-use-ocamlfind -use-menhir \
		-pkg atdgen -pkg cohttp.lwt \
		-pkg wcs-lib -pkg wcs-api \
		$@

%_t.ml %_t.mli: %.atd
	atdgen -t $<

%_j.ml %_j.mli: %.atd
	atdgen -j -j-std $<

clean:
	$(OCAMLBUILD) -clean

cleanall: clean
	rm -f $(RMAIN)
	rm -f *~ */*~ $(GENERATED)
