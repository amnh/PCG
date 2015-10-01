language = hs
main     = PackedTest
optimize = -O2
linking  = -main-is $(main)
warnings = -w -W -Wall
sanity   = -fwarn-duplicate-exports -fwarn-incomplete-patterns -fwarn-missing-signatures -fwarn-overlapping-patterns -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches -fwarn-unused-do-bind 
otherDir = -i"../../phylocomgraph":"." 
name     = $(main)
source   = $(main).$(language)

all: $(name)

$(name): $(source)
	ghc $< -o $@ $(optimize) $(linking) $(warnings) $(sanity) $(otherDir)
clean:
	rm -R *.o *.hi

parclean:
	rm BitPackedNode.o BitPackedNode.hi PackedTest.o PackedTest.hi PackedBuild.o PackedBuild.hi PackedOptimize.o PackedOptimize.hi PackedTest
