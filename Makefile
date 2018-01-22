TARGET = smtdump_test

all: $(TARGET).native

$(TARGET).native: *.ml
	ocamlbuild  $@

clean:
	rm -rf _build/ *~  *.output *.smt2 *.dot  $(TARGET).native
