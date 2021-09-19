
.PHONY: all
.PHONY: clean

all: src/grovobuf

clean:
	rm src/grovobuf

src/grovobuf: src/grovobuf.nim 
	nim --run compile ./src/grovobuf.nim   
