CC = gcc
CFLAGS = -ggdb -Wall -DDEBUG
#CFLAGS = -Wall -O9 -fomit-frame-pointer -funroll-loops
#CFLAGS = -ggdb -Wall -DDEBUG -DDEBUGTRACE
#CFLAGS = -Wall -O6 -DLIMIT_TO_GOTOH
#CFLAGS = -Wall -O3
LIBS =

EXECS = ukk.alloc ukk.noalign ukk.checkp ukk.dpa
#        ukk.alloc.backloop ukk.checkp.backloop

all: test_powell

ukk.checkp: ukk.checkp.o ukkCommon.o
	$(CC) ukk.checkp.o ukkCommon.o -o ukk.checkp $(LIBS)

ukk.alloc: ukk.alloc.o ukkCommon.o
	$(CC) ukk.alloc.o ukkCommon.o -o ukk.alloc $(LIBS)

ukk.noalign: ukk.noalign.o ukkCommon.o
	$(CC) ukk.noalign.o ukkCommon.o -o ukk.noalign $(LIBS)

ukk.checkp.backloop: ukk.checkp.backloop.o ukkCommon.o
	$(CC) ukk.checkp.backloop.o ukkCommon.o -o ukk.checkp.backloop $(LIBS)

ukk.alloc.backloop: ukk.alloc.backloop.o ukkCommon.o
	$(CC) ukk.alloc.backloop.o ukkCommon.o -o ukk.alloc.backloop $(LIBS)

ukk.dpa: ukk.dpa.o ukkCommon.o
	$(CC) ukk.dpa.o ukkCommon.o -o ukk.dpa $(LIBS)

tarball:
	./tar.pl align3str_checkp.tar.gz align3str_checkp README COPYRIGHT Makefile ukk.alloc.c ukk.checkp.c ukk.dpa.c ukkCommon.h ukkCommon.c

clean:
	rm -f *.o core $(EXECS) align3str_checkp.tar.gz

ukk.checkp.o: ukk.checkp.c ukkCommon.h
ukk.alloc.o: ukk.alloc.c ukkCommon.h
ukk.dpa.o: ukk.dpa.c ukkCommon.h

ukk.noalign.o: ukk.alloc.c ukkCommon.h
	$(CC) -c $(CFLAGS) -o ukk.noalign.o -DEDITCOST_ONLY ukk.alloc.c

.c.o:
	@echo ......................................Compiling $< to $@
	$(CC) -c $(CFLAGS) -o $@ $<

######### Eric added this to try to get the damn thing to compile.
test_powell : ukk.checkp.c ukkCommon.c ukkCommon.h
	gcc -std=c11 -g -c ukk.checkp.c
	gcc -std=c11 -g ukkCommon.c ukk.checkp.o -o testPowell