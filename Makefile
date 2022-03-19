##############################################################################
#                                                                            #
#  This file is part of fontforge-of-ocaml library                           #
#                                                                            #
#  Copyright (C) 2017-2022, Patrick BAUDIN                                   #
#                           (https://github.com/pbaudin/fontforge-of-ocaml)  #
#                                                                            #
#  you can redistribute it and/or modify it under the terms of the GNU       #
#  Lesser General Public License as published by the Free Software           #
#  Foundation, version 2.1.                                                  #
#                                                                            #
#  It is distributed in the hope that it will be useful, but WITHOUT ANY     #
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or         #
#  FITNESS FOR A PARTICULAR PURPOSE.                                         #
#                                                                            #
#  See the GNU Lesser General Public License version 2.1                     #
#  for more details (enclosed in the file licenses/LGPLv2.1)                 #
#                                                                            #
##############################################################################

.PHONY: all build clean

PACKAGE=fontforge-of-ocaml

all: $(PACKAGE).opam build

build:
	dune build @install

$(PACKAGE).opam: $(PACKAGE).opam.template dune-project
	rm -f $@
	dune build $@

PHONY: clean
clean:
	rm -fr _build

##############################################################################

.PHONY: tests
tests:
	dune build @runtest

##############################################################################

.PHONY: install uninstall

FFOO_INSTALLDIR?=""

install:
ifeq ($(FFOO_INSTALLDIR),"")
	dune install
else
	dune install --prefix ${FFOO_INSTALLDIR}
endif

uninstall:
ifeq ($(FFOO_INSTALLDIR),"")
	dune uninstall
else
	dune uninstall --prefix ${FFOO_INSTALLDIR}
endif

##############################################################################

.PHONY: headers

HEADER_FILES:=dune dune-project headers/headache_config.txt Makefile
HEADER_FILES+=src/dune $(wildcard src/*.ml) $(wildcard src/*.mli)
HEADER_FILES+=tests/dune tests/Makefile $(wildcard tests/*.ml) $(wildcard tests/*.mli)

HEADACHE=headache -c headers/headache_config.txt -h headers/license.txt

headers:
	$(HEADACHE) $(HEADER_FILES)

##############################################################################
