optimize      = -O2
linking       = --make
warning       = -w -W -Wall -Werror
sanity        = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-tabs -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind
stack         = -prof -auto-all -caf-all
bench-packing = packing-benchmark
all-targets   = $(bench-packing)

all: $(all-targets)

$(bench-packing): Packing/PackedTest.hs
	ghc $< -o $@ -main-is Packing.PackedTest $(optimize) $(linking) $(warnings) $(sanity) 
clean:
	find . -type f -name '*.o' -delete -print && find . -type f -name '*.hi' -delete -print && rm $(all-targets)

packClean:
	rm Packing/BitPackedNode.o Packing/BitPackedNode.hi Packing/PackedBuild.o Packing/PackedBuild.hi Packing/PackedOptimize.hi Packing/PackedOptimize.o Packing/PackedTest.hi Packing/PackedTest.o packing-benchmark
	
