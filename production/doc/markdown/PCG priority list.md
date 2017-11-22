### Priority list

**We have 24 weeks. Time requirements for below are omitted.**

1. 1.0 Release
    * Sankoff
        * efficiency (Goloboff)
        * testing

    * Testing aligned data types

    * Output
        * finalize decisions
        * extra GraphViz options?

    * Code ready for public consumption
        * better Haddock coverage
        * Mac compilation issues
        * tutorial

    * Reworking metadata <span style="color: teal">Might be hard</span>

    * 3D traversal inference <span style="color: darkorange">depends on redoing metadata</span>

    * Various recomputation efficiencies <span style="color: darkorange">depends on reworking metadata</span>
        * Sankoff (Goloboff)
        * others?
        * testing

    * Test 3D <span style="color: darkorange">depends on 3D traversal inference</span>

    * Code ready for public consumption
        * better Haddock coverage
        * platform-specific compilation instructions
        * tutorial

    * Graph interface <span style="color: teal">Decision: monadic?</span>
        * motivation: to allow simple operations w/o digging through
        * stabilize it
        * manipulation of both trees and networks (SPR, TBR, genetic algorithms, perturbation analysis)

    * Testing aligned data types

    * Already done:

        * input types

        * basic GraphViz output

        * build

        * post- and pre- order passes

1. Data structure tests
    * \\(\frac{1}{3}\\) are obvious
    * other straightforward stuff (but what?)
    * integration testing <span style="color: darkorange">easier with graph interface</span>


1. Publication (partially ordered) <span style="color: darkorange">depends on code & output</span>

    i. Network optimization
        * Dependencies:
            * Data structure tests
                * \\(\frac{1}{3}\\) are obvious
                * other straightforward stuff (but what?)
                * integration testing (easier with graph interface)

    i. IA
        * GPU stuff?

    i. Software description

    i. Trajectory stuff (edits on networks)
        * depends on SPR, TBR

    i. Single cost (pre-order, post-order for networks)

        * need this code locked down
            * `Maybe`s <span style="color: teal">might be hard</span>
            * verify single assignment
            * display trees for individual blocks or characters <span style="color: teal">might be hard</span>

    i. Various empirical cases
        * co-phylogenies
        * horizontal gene transfer
        * hybridization
        * hybridization on languages

1. Other stuff
    * GPU stuff

    * GUI

    * Test on various hardware

    * Sequential Alignment
