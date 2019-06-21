<style>
.reveal section img { background:none; border:none; box-shadow:none; }
.w { color: white; }
</style>

<!-- .slide: data-background="#134EA2" -->

## <span class="w"> Phylogenetic Software </span> </br> <span class="w"> in Haskell </span>

<span class="w"> Alex Washburn, Callan McGill </span> </br>
<span class="w"> Ward Wheeler </span>

![American Museum of Natural History](amnh-logo.png)

---

 * What is phylogenetics

 * What is the phylogenetic user story

 * How to organize a large scale Haskell project

 * Computaional challenges and Haskell

---

### What is phylogenetics?

 * Taxon set

 * Character observations

 * Binary trees

Note:
What is the most likey explaination of how a collection of animals are related to each other given a set of observations.

Note:
Binary trees, hirerarchical

---

### What is parsimony?

 * Occam's razor

 * Optimality criterion

Note: Occam's razor, which states that—all else being equal—the simplest hypothesis that explains the data should be selected

Note: Minimizes the total number of character-state changes is to be preferred

---

### What is tree space?

 * Tree Alignment Problem

---

### Phylogenetic Component Graphs (PCG)

 * Arbitrary taxa, not just organisms

 * Abstract characters, not just biological observations

 * Networks, not just binary trees

---

### Why Haskell?

 * Parellelism

 * Expressivity

 * Correctness

---

### Phylogenetic user story

 * Define taxa

 * Collect & colate character observations

 * Define your search parameters

 * Start your search

---

### Mechanically that looks like

 * Create a lot of large files

 * Set a lot of configuration options

 * Wait a very long time

---

### Scientists are expert users

 * Identify definate or potential issues

 * Collect and colate all issues

 * Suggest holistic solutions
