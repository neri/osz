TARGET   = 8086run
CC       = gcc
CXX      = g++
CFLAGS   = -O1 -Wall -g
CXXFLAGS = $(CFLAGS)
LDFLAGS  =
SOURCES  = bios.cpp debug.cpp interp.cpp io.cpp keyboard.cpp main.cpp tty.cpp utils.cpp
OBJECTS  = $(SOURCES:.cpp=.o)

all: $(TARGET) $(COMS)

$(TARGET): $(OBJECTS)
	$(CXX) -o $@ $(OBJECTS) $(LDFLAGS)

.SUFFIXES: .cpp.o
.cpp.o:
	$(CXX) -c $(CXXFLAGS) $<

clean:
	rm -f $(TARGET) $(TARGET).exe *.o *core

depend:
	rm -f dependencies
	for cpp in $(SOURCES); do \
		g++ -MM $(CXXFLAGS) $$cpp >> dependencies; \
	done

-include dependencies
