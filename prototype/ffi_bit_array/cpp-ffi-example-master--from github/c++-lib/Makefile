CXXFLAGS=-fPIC -std=c++11 -O3

LIBSHARED=libtst-c++.so

LIBSRCS=A.cpp
LIBOBJS=$(LIBSRCS:.cpp=.o)

all: $(LIBSHARED)

$(LIBSHARED): $(LIBOBJS)
	g++ -shared $(LIBOBJS) -o $(LIBSHARED)

install:
	mkdir -p /usr/local/c++-ffi-example
	mkdir -p /usr/local/c++-ffi-example/include
	mkdir -p /usr/local/c++-ffi-example/lib
	cp A.h /usr/local/c++-ffi-example/include
	cp $(LIBSHARED) /usr/local/c++-ffi-example/lib

clean:
	-rm -fr $(LIBSHARED) $(LIBOBJS)
