
.PHONY: all
.PHONY: clean

all: src/grovobuf

clean:
	rm src/grovobuf

src/grovobuf: src/grovobuf.nim src/parser.nim src/translator.nim src/ast.nim
	nim --run compile ./src/grovobuf.nim   
