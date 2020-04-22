.PHONY: all clean run 8086run

RAKE = rake

all:
	$(RAKE)

clean:
	$(RAKE) clobber

run: all 8086run
	$(RAKE) run

8086run: tools/8086run/8086run

tools/8086run/8086run:
	(cd tools/8086run; make)

# love:
