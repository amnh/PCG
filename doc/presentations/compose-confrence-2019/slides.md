<style>
.reveal section img { background:none; border:none; box-shadow:none; }
.w { color: white; }

});
</style>

<!-- .slide: data-background="#134EA2" -->

## <span class="w"> Phylogenetic Software </span> </br> <span class="w"> in Haskell </span>

<span class="w"> Alex Washburn, Callan McGill </span> </br>
<span class="w"> Ward Wheeler </span>

![American Museum of Natural History](amnh-logo.png)

---

### What we'll be covering:

 * What is phylogenetics?

 * Who are the users?

 * How we organize a large scale Haskell project?

 * Challenges of Haskell?

---

### What is phylogenetics?


<img src="emoji-phylogenetics.jpg" height="50%" width="50%">

 <font color="FF4500">The phylogenetic tree of emoji parrots</font> 



----

 *  <font color ="F9FAE1"> <b> Taxon set </b> : </font> <font color="FF4500">emoji parrots</font>
  <p>
   <font color="#50CB78">
     <i> More generally: </i> Groups of organisms
     , languages etc.
   </font>
  </p>

 * <font color ="F9FAE1"> <b> Character observations </b> : </font>  <font color="FF4500">colour of feathers</font>
<p>
   <font color="#50CB78">
     <i>More generally: </i> Other morphological characteristics
     , DNA characters etc.
   </font>
  </p>


 * <font color ="F9FAE1"> <b> Binary tree </b> : </font>  <font color="FF4500"> hypothesized evolutionary tree of emoji parrots. </font>
<p>
   <font color="#50CB78">
   The <i> "best" </i> (see next slide) tree 
   to explain the relationships between this data. 
   </font>
  </p>
---

### What is parsimony?

 * Occam's razor

 * Optimality criterion

Note: 
Occam's razor, which states that—all else being equal—the simplest hypothesis that explains the data should be selected

Note: 
Minimizes the total number of character-state changes is to be preferred

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


---

### Challenges

<section>
	<pre><code data-trim data-noescape>
data Graph = Vector (IndexData e n)
	</code></pre>
</section>
