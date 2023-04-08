# Usage:
# - `make utop` to get an interactive OCaml prompt
# - `make` to build
# - `make clean` to clean up
# - `make clean && make` to check for warnings and run tests (e.g., for grading)
# - `make fmt` to check how your code should be formatted
# - `make dist` to create a "distribution" file to submit for your assignment

# Set `all` as the default target
.PHONY: default # Tell `make` that this target is not a file
default: all

# Include homework specific variables from the file hw.mk
include hw.mk

.PHONY: utop # Tell `make` that this target is not a file
utop:
	dune utop .

.PHONY: all # Tell `make` that this target is not a file
all:
	dune runtest .
	dune build @install
	test -L bin || ln -s _build/install/default/bin .

.PHONY: clean # Tell `make` that this target is not a file
clean:
# Remove files produced by dune.
	dune clean
# Remove directory created by us
	test -L bin && rm -f bin || true
# Remove remaining files/folders ignored by git as defined in .gitignore (-X).
	test -d .git && git clean -dfXq || true

.PHONY: fmt # Tell `make` that this target is not a file
fmt:
	dune build @fmt

.PHONY: dist # Tell `make` that this target is not a file
dist: clean
	test -d _build || mkdir _build
	COPYFILE_DISABLE=1 tar --exclude .git -X .gitignore -zcvf _build/$(HW).tar.gz .
	@echo
	@echo Distribution file created: _build/$(HW).tar.gz
