CSCALA := scalac
SOURCES := $(shell ls *.scala)
TARGETS := $(SOURCES:.scala=)

.PHONY: all clean

all: $(TARGETS)

clean:
	rm -f *.class

$(TARGETS): %: %.scala
	$(CSCALA) $^
