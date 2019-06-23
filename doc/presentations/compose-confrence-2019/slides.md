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

 * Optimality criterion

 * Occam's razor

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

### Challenges

<section>
	<pre><code data-trim data-noescape>
data Graph = Vector (IndexData e n)
	</code></pre>
</section>
