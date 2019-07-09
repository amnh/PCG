<style>
.reveal section img { background:none; border:none; box-shadow:none; }
.w { color: white; }

});
</style>

<!-- .slide: data-background="#134EA2" -->

## <span class="w"> Phylogenetic Software </span> </br> <span class="w"> in Haskell </span>

<span class="w"> Callan McGill, Alex Washburn </span> </br>
<span class="w"> Ward Wheeler </span>

![American Museum of Natural History](amnh-logo.png)

---

### Today's Talk:


 *  <font color ="FF7417"> What is phylogenetics? </font>

 * <font color ="FF7417"> Organisation of our project </font>

 * <font color ="FF7417"> Who are the software users? </font>

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
     , DNA characters etc.
   <i> Note </i>: Characters have some notion of "metric" or transition cost.
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
  <li> For characters with a discrete number of states such as DNA bases (<b> A-C-G-T </b>) there are well-known algorithms (<b>Fitch </b>) to solve this problem efficiently. </li>
  <li> For more general characters such as DNA sequences (<b> variable length strings of base pairs</b>), this is NP-hard. </li>
</ul>
  </font>
</p>
----

### Problems of phylogenetics (Cont.)

<p>
 <b> <font color ="F9FAE1"> Generalized Tree Alignment Problem (GTAP) : </font> </b>  <font color="FF7417"> Among all possible trees and all possible extensions find those which minimize the overall cost. </font>
</p>
 <font color="#50CB78"> (This is extra NP-hard.) </font>

---

### Our project: Phylogenetic Component Graph (PCG)

 * <font color="FF7417"> Completely available at:  </font> 
     
      <p align="center"> https://github.com/amnh/PCG </p>

 * <font color="FF7417"> Code statistics: </font>

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

<img src="project-overview.png" > 

----

###  PCG Novel Phylogenetic Features

 *  <font color ="FF7417"> Taxa are completely general. </font>

 *  <font color ="FF7417"> Characters are also unconstrained and intended to be extensible to new types of characters. </font>

 *  <font color ="FF7417"> The system allows Networks (<i> see next slide </i>), rather than just binary trees. </font>

----

<img src="phylogenetic-network.png" > 

 <font color="FF7417">Schematic Phylogenetic Network</font> 

---

### Phylogenetic user story

 *  <font color ="FF7417"> Define taxa </font>

 *  <font color ="FF7417"> Collect & colate character observations </font>

 *  <font color ="FF7417"> Define your search parameters </font>

 * <font color ="FF7417"> Start your search </font>

----

### Mechanically this looks like

 * <font color ="FF7417"> Create a lot of large files</font>

 * <font color ="FF7417"> Set a lot of configuration options </font>

 * <font color ="FF7417"> Wait a very long time</font>

---

### Error Handling

<font color ="FF7417"> Don't stop at the first error, collect all the errors in a given <i> phase </i>: </font>

    data  ErrorPhase
        = Inputing  -- ^ Cannot get information off the disk
        | Parsing   -- ^ Cannot understand the information
        | Unifying  -- ^ Collectively incoherent information
        | Computing -- ^ We messed up, our bad
        | Outputing -- ^ Cannot write out information to disk

----

### Error Handling 

<font color ="FF7417"> Collect errors using `validation` package: </font> 

<section><pre><code data-trim data-noescape>
    type
    
    data Either b a     = Left b    | Right a

    data Validation e a = Failure e | Success a
</code></pre></section>

    instance Applicative (Either b) where
        Right f <\*> r = fmap f r
        Left  e <\*> _ = Left e

    instance Semigroup e => Applicative (Validation e) where
        Success f  <\*> r = fmap f r
        Failure e1 <\*> b = Failure $ case b of
          Failure e2 -> e1 <> e2
          Success \_ -> e1

----

### I/O Errors

<font color ="FF7417"> Use a custom semigroup for I/O errors </font>

    newtype InputError = InputError (NonEmpty InputErrorMessage)
      deriving Semigroup

    data InputErrorMessage = 
          FileAlreadyInUse   FileSource
        | FileAmbiguous      FileSource (NonEmpty FileSource)
        | FileBadPermissions FileSource
        | FileEmptyStream    FileSource
        | FileUnfindable     FileSource

----

 <font color ="F9FAE1"> <b> Uninformative stock I/O exception message: </b> </font>

<font color ="FF7417">
<pre>
Exception: Could not open file 'parrots.fasta'
</pre>
</font>

 <font color ="F9FAE1"> <b> Informative tailored I/O error message </b> </font>

<font color ="FF7417">
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
</font>

</section>

---

### Parse Errors

 *  <font color ="FF7417"> Parse errors need to be informative too! </font>

 * <font color ="FF7417"> Share the parsing context with the user </font>

 * <font color ="FF7417"> We do this using the `megaparsec` package </font>

 </font>

----

<section>

<font color ="F9FAE1"> <b> Uninformative stock parse error message </b> </font>

<font color ="FF7417">
    <pre>
parrots.fasta+284:3375
Unexpected 'Q'
    </pre>
</font>

<font color ="F9FAE1"> <b> Informative customized parse error message </b> </font>

<font color ="FF7417">
    <pre>
Parse error in 'parrots.fasta'
Line 284, column 3375:
  In the taxon 'Apple-Parrot-Emoji',
    In the 33rd line of sequence data
      Unexpected DNA nucleotide 'Q'
      Expected 'A', 'C', 'G', 'T', '-', or a DNA IUPAC code
    </pre>
</font>
</section>

---

### Other challenges

 * <font color ="FF7417"> Memory pressure: A typical phylogenetic example might have ~10^5 leaves (taxa) and ~10^7 observations. </font>

 *  <font color ="FF7417"> Efficient data structures: Certain data structures are hard to implement (<i> more later </i>) or not implemented in the Haskell ecosystem. For example we currently uses constant-time access hashmaps from C++ (though we eventually plan to move this into Haskell)  </font>

----

 * <font color ="FF7417"> FFI black box witchcraft: Linker GHC panics! Space leaks from C! Ahhhh! </font>

 * <font color ="FF7417"> String and I/O woes: We use Pipes to do file streaming, LazyText and ShortText for string representations and TextShow for both debugging and user representations. </font>

----

 * <font color ="FF7417"> Lack of row polymorphism: We make heavy use of lenses and the following idiom:

<pre class="hljs" style="display: block; overflow-x: auto; padding: 0.5em; background: rgb(39, 40, 34) none repeat scroll 0% 0%; color: rgb(221, 221, 221);"><span class="hljs-class"><span class="hljs-keyword" style="color: rgb(249, 38, 114); font-weight: 700;">data</span> <span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">Foo</span> a b = <span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">Foo</span></span>
  { fieldOne :: a
  , fieldTwo :: b
  }
<span class="hljs-class">
<span class="hljs-keyword" style="color: rgb(249, 38, 114); font-weight: 700;">class</span> <span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">HasFieldOne</span> s t a b  | s -&gt; a, t -&gt; b, s b -&gt; t, t a -&gt; s <span class="hljs-keyword" style="color: rgb(249, 38, 114); font-weight: 700;">where</span></span>
  _fieldOne :: <span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">Lens</span> s t a b
<span class="hljs-class">
<span class="hljs-keyword" style="color: rgb(249, 38, 114); font-weight: 700;">instance</span> <span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">HasFieldOne</span> (<span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">Foo</span> <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">a</span> <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">b</span>) (<span class="hljs-type" style="color: rgb(166, 226, 46); font-weight: 700;">Foo</span> <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">a'</span> <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">b</span>) a a'
  _fieldOne = lens fieldOne (\<span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">r</span> <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">f</span> -&gt; <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">r</span> {<span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">fieldOne</span> = <span class="hljs-title" style="color: rgb(166, 226, 46); font-weight: 700;">f</span>})</span></pre>

---

### Example: Graph representation:

<font color="#50CB78"> <i> Things we want from our graph representation: </i> </font>

 *  <font color ="FF7417"> The graphs we allow are (roughly) rooted networks (<i> as depicted earlier </i>). </font>

 * <font color ="FF7417"> Fast look-up on nodes. </font>

 * <font color ="FF7417"> Ability to work with subgraphs. </font>
 
 *  <font color ="FF7417"> Ability to traverse from leaf to root and from root to leaf in linear time. This is similar to a doubly linked tree. This rules out representations in terms of Binary trees or rose trees or etc. </font>


----

### Currently how this (roughly) looks

  <font color="#50CB78"> <i> NB: This is about to be re-designed. </i> </font>


<section>
	<pre><code data-trim data-noescape>
data Graph = Vector (IndexData e n)

data IndexData e n = IndexData
  { nodeLabel  :: n
  , childRefs  :: IntMap e
  , parentRefs :: IntSet 
  }
	</code></pre>
</section>

 <font color ="FF7417"> This leads to a lot of problems with index errors and also with memoized traversals. </font>

----

### Memoized single-pass traversals

<font color ="FF7417"> In order to do single traversals (à la beautiful folds):

<section>
	<pre><code data-trim data-noescape>

newtype MemoVector a = MemoVector (Endo (Int -> a))


 generateMemo :: forall a
  .  Int           -- ^ Range of memoization
  -> MemoVector a  -- ^ Unmemoized function with open recursion
  -> Vector a      -- ^ Memoized vector

class Cartesian f where
    pair :: f a -> f b -> f (a, b)


class ExpFunctor f where
    xmap :: (a -> b) -> (b -> a) -> f a -> f b

instance Cartesian MemoVector where
  [...]
instance ExpFunctor MemoVector where
  [...]

	</code></pre>
</section>

<font color ="FF7417"> This allows us to build combinations of memoized vectors which are computed
in a single pass over a graph. </font>

---

### Project funding provided by:

  * American Museum of Natural History

  * DARPA SIMPLEX

  * Kleberg Foundation

---

## Thank you for listening!
