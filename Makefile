# $Id: Makefile 4119 2016-11-15 21:40:45Z sutre $

# Configuration
BUILD_FLAGS = -use-ocamlfind -use-menhir
DOC_FLAGS   = -charset UTF-8, -stars, -colorize-code, -sort,
DOC_FLAGS  += -hide Pervasives, -hide-warnings
REPORT = report_Guemon-Amelie
REPDIR = report

# Default target
all: bmc.d.byte

%.native %.byte %.d.byte %.cmo %.d.cmo: FORCE
	ocamlbuild $(BUILD_FLAGS) $@

doc: FORCE
	ocamlbuild $(BUILD_FLAGS) -docflags '$(DOC_FLAGS)' doc.docdir/index.html
	mv doc.docdir doc

test: FORCE runtests.d.byte
	CAMLRUNPARAM="b" ./runtests.d.byte

report: FORCE
	@(cd $(REPDIR) && pdflatex $(REPORT).tex)
	@(cd $(REPDIR) && ./.pdf_opener.sh $(REPORT).pdf &)

clean: FORCE
	ocamlbuild -quiet -clean
	@(for d in */; do \
		cd $$d && rm *~ 2> /dev/null; \
		cd ..; \
	done)

clean-report: FORCE
	@(cd $(REPDIR) && rm -rf *~ *.aux *.out *.log 2> /dev/null)
	@(echo 'Report cleaned-up')

FORCE:

# Check for ocamlbuild
ifneq ($(shell ocamlbuild -version 2>/dev/null | sed -e 's/ .*//'), ocamlbuild)
  $(error Check for ocamlbuild failed.  Is OCaml installed?)
endif

# Check for menhir
ifneq ($(shell menhir --version 2>/dev/null | sed -e 's/,.*//'), menhir)
  $(error Check for menhir failed.  Is Menhir installed?)
endif

# Check for OCaml compiler >= 4.01.0 (required for Format.asprintf)
ifneq ($(shell expr $$(ocamlc -version) \>= 4.01.0), 1)
  $(error Check for ocamlc version >= 4.01.0 failed.)
endif
