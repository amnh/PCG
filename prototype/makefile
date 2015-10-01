optimize = -O2
linking  = --make
warning  = -w -W -Wall -Werror
sanity   = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind

all: packing-benchmark

packing-benchmark: Packing/PackedTest.hs
	ghc $< -o $@ -main-is Packing.PackedTest $(optimize) $(linking) $(warnings) $(sanity)
clean:
	rm -R *.o *.hi

cleanPack:
	rm Packing/BitPackedNode.o Packing/BitPackedNode.hi Packing/PackedTest.o Packing/PackedTest.hi Packing/PackedBuild.o Packing/PackedBuild.hi Packing/PackedOptimize.o Packing/PackedOptimize.hi packing-benchmark
