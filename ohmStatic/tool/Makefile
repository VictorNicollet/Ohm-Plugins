HERE=`pwd`
OHM=$(HERE)/../../../Ohm/
BUILD=ocamlbuild -use-ocamlfind -lib ohm -cflags -I,$(OHM) -lflags -I,$(OHM)

all:
	$(BUILD) run.byte
	cp run.byte run

clean:
	rm _build run.byte run
