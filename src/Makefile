#
# Build system for ocs library and interpreter
#

include Makefile.common

BC_LIB = ocs.cma
N_LIB = ocs.cmxa
C_LIB = ocs.a
INTERP = ocscm


OCS_SOURCES = ocs_error.ml ocs_port.ml ocs_vartable.ml ocs_sym.ml \
	ocs_env.ml ocs_char.ml ocs_complex.ml ocs_numaux.ml \
	ocs_num.ml ocs_numstr.ml ocs_lex.ml ocs_misc.ml ocs_read.ml \
	ocs_eval.ml ocs_compile.ml ocs_contin.ml ocs_print.ml \
	ocs_io.ml ocs_list.ml ocs_macro.ml ocs_prim.ml ocs_string.ml \
	ocs_vector.ml ocs_wrap.ml ocs_top.ml

BC_OBJS = $(OCS_SOURCES:.ml=.cmo)

N_OBJS = $(OCS_SOURCES:.ml=.cmx)

INTERP_OBJS = ocs_main.cmx

BCI_OBJS = ocs_main.cmo
BCI = ocscm-bc

all: $(BC_LIB) $(N_LIB) $(INTERP)

native: $(N_LIB)

bytecode: $(BC_LIB)

$(N_LIB): $(N_OBJS)
	$(OCAMLOPT) -a -o $@ $^

$(BC_LIB): $(BC_OBJS)
	$(OCAMLC) -a -o $@ $^

$(INTERP): $(N_LIB) $(INTERP_OBJS)
	$(OCAMLOPT) -o $@ nums.cmxa unix.cmxa $^

$(BCI): $(BC_LIB) $(BCI_OBJS)
	$(OCAMLC) $(OCAMLFLAGS) -o $@ nums.cma unix.cma $^

clean:
	-rm -f $(N_LIB) $(BC_LIB) $(C_LIB) $(INTERP) *.cm* *.o
	-rm -f $(BCI)
	-rm .depend

.depend: $(OCS_SOURCES) $(wildcard *.mli)
	$(OCAMLDEP) $^ > $@

-include .depend

