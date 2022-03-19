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

TOOL=test_FontForgeLib

all: tests

.PHONY: tools
tools: $(TOOL).exe

.PHONY: $(TOOL).exe
$(TOOL).exe:
	dune build $@

.PHONY: tests
# note: [dune build @runtest] performs the comparison with the expected results
tests:
	dune exec -- ./$(TOOL).exe

.PHONY: clean-tests
clean-tests:
	rm -f $(TOOL).ttf

.PHONY: clean
clean: clean-tests
	dune clean
