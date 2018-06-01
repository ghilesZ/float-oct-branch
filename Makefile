# An abstract fixpoint solver based on Constraint Programming
#
# Author: Antoine Mine
# Copyright 2014


#######

UNAME := $(shell uname)

CC =        gcc
OCAMLC =    ocamlc.opt
OCAMLOPT =  ocamlopt.opt
OCAMLDEP =  ocamldep
OCAMLDOC =  ocamldoc
OCAMLLEX =  ocamllex
MENHIR =    menhir


# external libs
###############

ifeq ($(UNAME), Linux)
ZARITHDIR = `ocamlfind query zarith`
APRONDIR = `ocamlfind query apron`
GMPDIR = `ocamlfind query gmp`
CAMLIDLDIR = `ocamlfind query camlidl`
else
ZARITHDIR = +../pkg-lib/zarith
APRONDIR = +../pkg-lib/apron
GMPDIR = +../pkg-lib/gmp
endif

# flags & paths
###############

OCAMLDIR = `$(OCAMLC) -where`
OCAMLFLAGS = -g -bin-annot
OCAMLOPTFLAGS = -bin-annot
OCAMLINC = -I libs -I frontend -I solver -I $(ZARITHDIR) -I $(APRONDIR) -I $(GMPDIR) -I $(CAMLIDLDIR)
OCAMLLIBS = bigarray.cma gmp.cma apron.cma octD.cma polkaMPQ.cma zarith.cma str.cma unix.cma -cclib "-L$(ZARITHDIR) -L$(APRONDIR) -L$(GMPDIR) -L$(CAMLIDLDIR)"
OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa octD.cmxa polkaMPQ.cmxa zarith.cmxa str.cmxa unix.cmxa -cclib "-L$(ZARITHDIR) -L$(APRONDIR) -L$(GMPDIR) -L$(CAMLIDLDIR)"
MENHIRFLAGS = --explain
CFLAGS = -I $(OCAMLDIR) -O3 -Wall
CLIBS = -lgmp
DATE = `date +'%Y%m%d'`
VERSION = 1.0

DIST = icp-$(VERSION)

ifeq ($(DEBUG),1)
OCAMLFLAGS := $(OCAMLFLAGS) -g
OCAMLOPTFLAGS := $(OCAMLOPTFLAGS) -g
CFLAGS := $(CFLAGS) -g
endif



# files
#######

ifeq ($(UNAME), Linux)
SOLVE = solve
SOLVEB = solve.b
else
SOLVE = solve.exe
SOLVEB = solve.b.exe
endif

TARGETS = $(SOLVE) $(SOLVEB)

MLFILES = \
  frontend/syntax.ml \
  frontend/parser.ml \
  frontend/lexer.ml \
  frontend/file_parser.ml \
	libs/mystdlib.ml \
	libs/id.ml \
  libs/mapext.ml \
  libs/bot.ml \
  libs/bound_sig.ml \
  libs/bound_float.ml \
  libs/bound_rational.ml \
  libs/itv_sig.ml \
  libs/itv.ml \
  libs/abstract_sig.ml \
  libs/abstract_box.ml \
  libs/abstract_apron.ml \
  libs/svg.ml \
  solver/parameters.ml \
  solver/absinterp.ml \
  solver/old_soup.ml \
  solver/soup.ml \
  solver/solver.ml

MLIFILES = \
  libs/mapext.mli \
  libs/bot.mli

CFILES = \
  libs/ml_float.c

AUTOGEN = \
  frontend/lexer.ml \
  frontend/parser.ml \
  frontend/parser.mli

CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)
CMIFILES = $(MLIFILES:%.ml=%.cmi)
OFILES = $(CFILES:%.c=%.o)


# rules
#######

all: $(SOLVE)

$(SOLVE): $(OFILES) $(CMXFILES)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

$(SOLVEB): $(OFILES) $(CMOFILES)
	$(OCAMLC) -custom -o $@ $(OCAMLFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLLIBS) $+

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.ml

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC)  -c $*.ml

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(MENHIR) $(MENHIRFLAGS) $*.mly

clean:
	rm -f depend $(AUTOGEN) $(TARGETS)
	rm -f `find . -name "*.conflicts"`
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f out/*

MLSOURCES = $(MLFILES) $(MLIFILES)

depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native -slash $(OCAMLINC) $(MLSOURCES) | sed 's=C:=/cygdrive/c=' > depend


.phony:	all clean

include depend
