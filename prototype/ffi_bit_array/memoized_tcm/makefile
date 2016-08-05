CXX = g++
CXXFLAGS += -I/opt/local/include
LDFLAGS += -L/opt/local/lib
SOURCES = memoized_tcm.cpp
OBJECTS = $(SOURCES:.cpp=.o)
TARGETS = a.out

$(TARGETS) : $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o $@ $^ $(LDFLAGS)
