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

### Today's Talk:


 *  <font color ="FF7417"> What is phylogenetics? </font>

 * <font color ="FF7417"> Who are the users? </font>

 * <font color ="FF7417"> Organisation of our project </font>

 * <font color ="FF7417"> Challenges of Haskell </font>


---

### What is phylogenetics?


<img src="emoji-phylogenetics.jpg" height="50%" width="50%">

 <font color="FF7417">The phylogenetic tree of emoji parrots</font> 



----

 *  <font color ="F9FAE1"> <b> Taxon set </b> : </font> <font color="FF7417">emoji parrots</font>
  <p>
   <font color="#50CB78">
     <i> More generally: </i> Groups of organisms
     , languages, etc.
   </font>
  </p>

 * <font color ="F9FAE1"> <b> Character observations </b> : </font>  <font color="FF7417">colour of feathers</font>
<p>
   <font color="#50CB78">
     <i>More generally: </i> Other morphological characteristics
     , DNA characters etc. <i> Note </i>: Characters have some notion of "metric" or transition cost.
   </font>
  </p>


 * <font color ="F9FAE1"> <b> Binary tree </b> : </font>  <font color="FF7417"> hypothesized evolutionary tree of emoji parrots. </font>
  <p>
   <font color="#50CB78">
    <i> More generally: </i> The <i> "best" </i> (see next slide) tree 
   to explain the relationships between these data. 
   </font>
  </p>

----

### Problems of phylogenetics
<p>
 <b> <font color ="F9FAE1"> Tree Alignment Problem (TAP) : </font> </b>  <font color="FF7417"> Given a tree and a set of characters, find an extension of those characters to the internal nodes in the tree which is minimal amongst all such extensions. <i> Minimality </i> here means reducing the overall transition cost over the entire tree. </font>
</p>

----

### Remarks:

<p> <font color="#50CB78">
<ul>
  <li> For characters with a discrete number of states such as DNA bases (<b> A-C-G-T </b>) there are well-known algorithms (<b>Fitch </b>) to solve this problem in reasonable time. </li>
  <li> For more general characters such as DNA sequences (<b> variable length strings of base pairs</b>), this is an NP-hard problem. </li>
</ul>
  </font>
</p>
----

### Problems of phylogenetics (Cont.)

<p>
 <b> <font color ="F9FAE1"> Generalized Tree Alignment Problem (GTAP) : </font> </b>  <font color="FF7417"> Find the tree and an extension of characters to that tree which is minimal amongst all possible trees. </font>
</p>
 <font color="#50CB78"> <i> (This is extra NP-hard.) </i> </font>

---

### Phylogenetic Component Graphs (PCG)

 * Taxa are completely general.

 * Character are not necessarily just biological observations but are general and extensible to new types of characters.

 * The system allows Networks, rather than just binary trees

---

### Phylogenetic user story

 * Define taxa

 * Collect & colate character observations

 * Define your search parameters

 * Start your search

----

### Mechanically that looks like

 * Devlop a falsifiable hypothesis

 * Create a lot of large files

 * Set a lot of configuration options

 * Wait a very long time

---

### Why Haskell for PCG?

 * Parellelism

 * Expressivity

 * Correctness

---

### Scientists are expert users

 * Identify definite or potential issues

 * Collect and colate all issues

 * Suggest holistic solutions

---

Don't stop at the first error

<section>
	<pre><code data-trim data-noescape>
data  ErrorPhase
    = Inputing  -- ^ Cannot get information off the disk
    | Parsing   -- ^ Cannot understand the information
    | Unifying  -- ^ Collectively incoherent information
    | Computing -- ^ We messed up, our bad
    | Outputing -- ^ Cannot write out information to disk
	</code></pre>
</section>

Collect all errors in a given phase

----

Use the `validation` package

<section> <pre><code data-trim data-noescape>
data  Validation err a
    = Failure err
    | Success a

instance Semigroup err => Applicative (Validation err) where

    pure = Success

    Failure e1 <\*> b = Failure $ case b of
      Failure e2 -> e1 <> e2
      Success \_ -> e1
    Success \_  <\*> Failure e2 = Failure e2
    Success f  <\*> Success a  = Success (f a)
</code></pre></section>

---

Use a custom semigroup for I/O errors

<section> <pre><code data-trim data-noescape>
newtype InputStreamError =
        InputStreamError (NonEmpty InputStreamErrorMessage)

data  InputStreamErrorMessage
    = FileAlreadyInUse   FileSource
    | FileAmbiguous      FileSource (NonEmpty FileSource)
    | FileBadPermissions FileSource
    | FileEmptyStream    FileSource
    | FileUnfindable     FileSource

instance Semigroup InputStreamError where

    (InputStreamError lhs) <> (InputStreamError rhs) =
        InputStreamError $ lhs <> rhs
</code></pre></section>

----

<section>

Uninformative stock I/O exception message

<pre>
Exception: Could not open file 'parrots.fasta'
</pre>

Informative tailored I/O error message

<pre>
The file 'parrots.fasta' is already in use.

The following files do not exist:
  cryptozoology/big-foot.fasta
  cryptozoology/dragon.fasta
  cryptozoology/loch-ness.fasta
  cryptozoology/unicorn.fasta
  cryptozoology/yeti.fasta

The file 'void.data' was empty.

The following files have permissions which prevent them from being
opened:
  hidden.log
  secret.txt
</pre>

</section>

---

Parse errors need to be informative too!

Share the parsing context with the user

Use the `megaparsec` package

----

<section>

Uninformative stock parse error message

    <pre>
parrots.fasta+284:3375
Unexpected 'Q'
    </pre>

Informative customized parse error message

    <pre>
Parse error in 'parrots.fasta'
Line 284, column 3375:
  In the taxon 'Apple-Parrot-Emoji',
    In the 33rd line of sequence data
      Unexpected DNA nucleotide 'Q'
      Expected 'A', 'C', 'G', 'T', '-', or a DNA IUPAC code
    </pre>

</section>

---

### Ergonomic user experiance

---

### Our project: Phylogenetic Component Graph

 * Completely available at: 
     
      <p align="center"> https://github.com/amnh/PCG </p>

 * Code statistics:

 <table style="width:5%" "height:5%">
  <tr>
    <th>Language</th>
    <th>Files</th>
    <th>Lines</th>
    <th>Code</th>
  </tr>
  <tr>
    <td>Haskell</td>
    <td>380</td>
    <td>64210</td>
    <td>37225</td>
  </tr>
  <tr>
    <td>C</td>
    <td>34</td>
    <td>23378</td>
    <td>14578</td>
  </tr>
  <tr>
    <td>C++</td>
    <td>4</td>
    <td>887</td>
    <td>533</td>
  </tr>
</table> 


----

### Project Layout


----

### Things we make heavy use of:


---

### Challenges of Haskell

 * Memory pressure: A typical phylogenetic example might have ~10^5 leaves (taxa) and ~10^7 observations.

 * Efficient data structures: Certain data structures are hard to implement (see next slide) or not implemented in the Haskell ecosystem. For example we currently uses Hashmaps from C++ as constant-time access Hashmaps were not implemented in Haskell at the time.

----

 * FFI black box witchcraft: Linker GHC panics! Space leaks! Ahhh!

 * String woes: We use Pipes to do file streaming, LazyText and ShortText for string representations and TextShow for both debugging and user representations.

 * Profiling: It can be hard to 
 
----

### Example: Graph representation:

Things we want on our graph:

 * The graphs we allow are (roughly) rooted networks.
 
 * Ability to traverse from leaf to root and from root to leaf in linear time.

 * Fast look-up on nodes.

 * Ability to work with subgraphs.

----



<section>
	<pre><code data-trim data-noescape>
data Graph = Vector (IndexData e n)
	</code></pre>
</section>

<section>
	<pre><code data-trim data-noescape>
newtype DVector a = DVector { getDVector :: Endo (Int -> a)}
	</code></pre>
</section>
----

 * This is nice as far as it goes but also frequently a great way to shoot yourself in the foot.
