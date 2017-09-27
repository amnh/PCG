(** General functions *)

(** {2 Functions on Strings} *)

(** Return the first character of a string, should it have one. Otherwise return
[None]. *)
val firstchar : string -> char option

(** Return the first character of a string, should it have one. Otherwise return
[None]. *)
val lastchar : string -> char option

(** Map a string onto itself, character-by-character using the given function.
*)
val string_selfmap : (char -> char) -> string -> unit

(** List of characters representing a string. *)
val explode : string -> char list

(** String representing a list of characters. Fails if list is longer than
[Sys.max_string_length]. *)
val implode : char list -> string

(** Make a string from a character. *)
val string_of_char : char -> string

(** {2 Functions on lists} *)

val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val rev : 'a list -> 'a list
val iter : ('a -> unit) -> 'a list -> unit
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val iter3 : ('a -> 'b -> 'c -> unit) -> 'a list -> 'b list -> 'c list -> unit
val append : 'a list -> 'a list -> 'a list
val ( @ ) : 'a list -> 'a list -> 'a list
val flatten : 'a list list -> 'a list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val map : ('a -> 'b) -> 'a list -> 'b list
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val split : ('a * 'b) list -> 'a list * 'b list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val split8 : ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) list ->
  'a list * 'b list * 'c list * 'd list * 'e list * 'f list * 'g list * 'h list
val combine : 'a list -> 'b list -> ('a * 'b) list
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val length : 'a list -> int
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Tail-recursive versions of list functions. See [Pervasives] for documentation. *)

val cumulative_sum : int -> int list -> int list

val lcount : ('a -> bool) -> 'a list -> int

val option_map : ('a -> 'b option) -> 'a list -> 'b list

(** Synonym for [List.mem]. *)
val member : 'a -> 'a list -> bool

(** Similar to [rev_map], but 3 arguments. *)
val rev_map3 :
  ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(** Similar to [map2], but 3 arguments. *)
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

(** Similar to [rev_map], but 4 arguments. *)
val rev_map4 :
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list

(** Similar to [map2], but 4 arguments. *)
val map4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a list -> 'b list -> 'c list -> 'd list -> 'e list 

(** Similar to [rev_map], but 5 arguments. *)
val rev_map5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list

(** Similar to [map2], but 5 arguments. *)
val map5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a list -> 'b list -> 'c list -> 'd list -> 'e list -> 'f list

(** [take l n] takes [n] elements from the list raising [Invalid_argument] if
there are not enough elements to take or if n < 0. *)
val take : 'a list -> int -> 'a list

(** Same, arguments reversed. *)
val take' : int -> 'a list -> 'a list

(** [drop l n] drops [n] elements from the list raising [Invalid_argument] if n
 < 0 or there are not enough elements. *)
val drop : 'a list -> int -> 'a list

(** Same, arguments reversed. *)
val drop' : int -> 'a list -> 'a list

(** take elements from a list while a given predicate is true. *)
val takewhile : ('a -> bool) -> 'a list -> 'a list

(** drop elements from a list while a given predicate is true. *)
val dropwhile : ('a -> bool) -> 'a list -> 'a list

(** [cleave l n] splits [l] into two parts, returned as a tuple. The first
 contains the first [n] elements, the second the remainder.
 Order is preserved. [Invalid_argument] is raised on negative argument or not
 enough elements in list. *)
val cleave : 'a list -> int -> 'a list * 'a list

(** Same, but split point controlled by a predicate, which is true for elements
in the first returned list. e.g [cleavewhile even [2;4;5;6] ==> ([2;4], [5;6])] *)
val cleavewhile : ('a -> bool) -> 'a list -> 'a list * 'a list

(** Isolate a central section of a list, from the first element after the element
for which predicate [p] is true, to the element before [p'] is first true. *)
val isolate : ('a -> bool) -> ('a -> bool) -> 'a list -> 'a list

(** Interleave an element among a list, so that [interleave 0 [1; 2; 3]]
yields [[1; 0; 2; 0; 3]]. An empty or singleton list is unchanged. *)
val interleave : 'a -> 'a list -> 'a list

(** Interleave two equal-length lists, taking from the first list first. *)
val interleave_lists : 'a list -> 'a list -> 'a list

(** Collate a list into a list of lists based upon a comparison function by which
it has already been sorted. e.g [collate [1; 2; 2; 3; 3]] calculates
[[[1]; [2;2]; [3;3]]]. *)
val collate : ('a -> 'a -> int) -> 'a list -> 'a list list

(** Map on lists of lists. So [map_lol f] is [map (map f)]. *)
val map_lol : ('a -> 'b) -> 'a list list -> 'b list list

(** Produce a list of overlapping pairs of elements in a list in order, producing
the empty list if on singleton input. e.g [pairs [1; 2; 3]] is [[(1, 2); (2,
3)]]. *)
val pairs : 'a list -> ('a * 'a) list

(** [List.mem] with arguments reversed. *)
val member' : 'a list -> 'a -> bool

(** Remove duplicates from a list. *)
val setify : 'a list -> 'a list

(** Same, but preserve order. *)
val setify_preserving_order : 'a list -> 'a list

(** The set [setminus a b] contains all those elements which are in [a] but are
do not appear in [b]. *)
val setminus : 'a list -> 'a list -> 'a list

(** Return a list of the heads of a list of lists, each of which has at least
one element, preserving order. *)
val heads : 'a list list -> 'a list

(** Ditto, tails. *)
val tails : 'a list list -> 'a list list

(** Take a list of lists of equal length, and turn into a list of lists, the
first containing all the first elements of the original lists, the second the
second, and so on. *)
val zipn : 'a list list -> 'a list list

(** Couple the elements of a list [l] using function [f]. For instance,
[couple ( + ) [[1; 3; 5]]] is [[4; 8]]. The two elements
are applied to [f] in the order in which they appear in the input list. *)
val couple : ('a -> 'a -> 'b) -> 'a list -> 'b list

(** As [couple], but an extra function [g] is applied to any last (odd) element. *)
val couple_ext : ('a -> 'a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list

(** Apply [couple] repeatedly until only one element remains. Return that
element. *)
val couple_reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

(** A similar function to [couple], but the coupling is non-overlapping. *)
val pair : ('a -> 'a -> 'a) -> 'a list -> 'a list

(** A version of [pair] which adds a unary function for the singleton, much
like [couple_ext]. *)
val pair_ext : ('a -> 'a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list

(** As [couple_reduce] is to [couple], so [pair_reduce] is to [pair]. *)
val pair_reduce : ('a -> 'a -> 'a) -> 'a list -> 'a

val keep : ('a -> bool) -> 'a list -> 'a list

(** [List.filter] has a confusing name, so we define [keep] and [lose] to avoid
error. *)
val lose : ('a -> bool) -> 'a list -> 'a list

(** [many x n] makes a list of length [n] with each element equal to [x]. *)
val many : 'a -> int -> 'a list

(** A version where we need to apply unit each time, for instance when producing
a list of random numbers. Result is ordered. *)
val manyunique : (unit -> 'a) -> int -> 'a list

(** Split a list into some lists of length [n] (and possibly a final one of
length [< n]), preserving order. *)
val splitinto : int -> 'a list -> 'a list list

val splitat : int list -> 'a list -> 'a list list

(** Select the nth element in a list (first is element 1). Raises
[Invalid_argument] if the number is out-of-range. *)
val select : int -> 'a list -> 'a

val null : 'a list -> bool

(** Predicates on the nullness of a list. *)
val notnull : 'a list -> bool

(** Find the last element of a list. Raises [Invalid_argument] on empty list. *)
val last : 'a list -> 'a

(** Produce a list containing all but the last element of a list. For the empty
list, returns the empty list. *)
val all_but_last : 'a list -> 'a list

(** Find the first and last element of a list. If the list has one element, that
is returned twice. If it has no elements, raise [Invalid_argument]. *)
val extremes : 'a list -> 'a * 'a

(** Return the first, middle and last elements of a list which has length at
least two. Otherwise, raise [Invalid_argument]. *)
val extremes_and_middle : 'a list -> 'a * 'a list * 'a

(** [ilist 2 5] returns [[2; 3; 4; 5]]. However, [ilist 5 2] raises [Invalid_argument]. *)  
val ilist : int -> int -> int list

val ilist_null : int -> int -> int list

val ilist_fail_null : int -> int -> int list

(** [indx l] returns [1; 2; 3] if [l] has length 3, for example. *)
val indx : 'a list -> int list

(** Same, but 0-based. *)
val indx0 : 'a list -> int list

(** Same, but n-based. *)
val indxn : int -> 'a list -> int list

(** Remove the second, fourth etc. elements from a list, saving the last element
(if of even length) e.g [drop_evens [1; 2; 3; 4; 5; 6]] is [[1; 3; 5; 6]]. *) 
val drop_evens : 'a list -> 'a list

val really_drop_evens : 'a list -> 'a list

(** Remove the first, third etc. The last odd element is not saved. e.g [drop_odds
[1;2;3;4;5;6;7]] is [[2;4;6]]. *)
val drop_odds : 'a list -> 'a list

(** Like [List.tl] but [[]] yields [[]]. *)
val tail_no_fail : 'a list -> 'a list

(** Append with reversed arguments. *)
val ( @@ ) : 'a list -> 'a list -> 'a list

(** [replaceinlist f x l] replaces any element of [l] for which [f l] is true
with [x]. *)
val replaceinlist : ('a -> bool) -> 'a -> 'a list -> 'a list

(** Find the position of the first element matching a predicate. The first
element is number one. Fails with [Not_found]. *)
val index : ('a -> bool) -> 'a list -> int

(** {2 32 and 64 bit integers} *)

val i32ofi : int -> int32
val i32toi : int32 -> int
val i32tof : int32 -> float
val i32add : int32 -> int32 -> int32
val i32sub : int32 -> int32 -> int32
val i32div : int32 -> int32 -> int32
val i32mul : int32 -> int32 -> int32
val lsr32 : int32 -> int -> int32
val lsl32 : int32 -> int -> int32
val lor32 : int32 -> int32 -> int32
val land32 : int32 -> int32 -> int32
val lxor32 : int32 -> int32 -> int32
val i32succ : int32 -> int32
val i32pred : int32 -> int32
val i32max : 'a -> 'a -> 'a
val i32min : 'a -> 'a -> 'a

val i64ofi : int -> int64
val i64toi : int64 -> int
val i64add : int64 -> int64 -> int64
val i64sub : int64 -> int64 -> int64
val i64mul : int64 -> int64 -> int64
val i64div : int64 -> int64 -> int64
val lsr64 : int64 -> int -> int64
val lsl64 : int64 -> int -> int64
val lor64 : int64 -> int64 -> int64
val i64succ : int64 -> int64
val i64pred : int64 -> int64
val i64max : 'a -> 'a -> 'a
val i64min : 'a -> 'a -> 'a

(** Shortened names for the obvious functions from [Int32] and [Int64]. *)

(** {2 Efficient Queues} *)

type 'a queue

(** The empty queue *)
val q_mk : 'a queue

(** Enqueue *)
val q_enq : 'a queue -> 'a -> 'a queue

(** Null predicate *)
val q_null : 'a queue -> bool

(** Raised upon dequeuing an empty queue. *)
exception EmptyQueue

(** Peek at the head of a queue *) 
val q_hd : 'a queue -> 'a

(** Remove an element from the queue *)
val q_deq : 'a queue -> 'a queue

val q_len : 'a queue -> int

val list_of_q : 'a queue -> 'a list

val q_of_list : 'a list -> 'a queue

(** {2 Bytestreams and Streams } *)

(** Big arrays of bytes. *)
type bytestream =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Make from a given size. *)
val mkstream : int -> bytestream

(** Size of a stream in bytes. *)
val stream_size : bytestream -> int

(** Fill a stream with a value *)
val fillstream : int -> bytestream -> unit

(** Print a bytestream. Format undefined: for debug only. *)
val print_stream : bytestream -> unit

(** Make a bytestream from a string, no terminator. *)
val bytestream_of_string : string -> bytestream

(** Make a bytestream from an integer list. *)
val bytestream_of_list : int list -> bytestream

(** Make a bytestream from a character list. *)
val bytestream_of_charlist : char list -> bytestream

(** Make a bytestream from a list of integer arrays. *)
val bytestream_of_arraylist : int array list -> bytestream

(** And from an integer array *)
val stream_of_int_array : int array -> bytestream

(** An integer array of bytes from a stream *)
val int_array_of_stream : bytestream -> int array

(** Integer array from a string *)
val int_array_of_string : string -> int array

(** A string from a list of integer arrays *)
val string_of_int_arrays : int array list -> string

(** A string from a single int array *)
val string_of_int_array : int array -> string

(** A string from a bytestream *)
val string_of_stream : bytestream -> string

(** Make a string from a bytestream. Fails if array is longer than
[String.max_length]. *)
val string_of_bytestream : bytestream -> string

(** Make a character list from a byte stream *)
val charlist_of_bytestream : bytestream -> char list

(** Copy a bytestream. *)
val copystream : bytestream -> bytestream

(** {2 Association lists} *)

(** [lookup x l] looks up something, returning [None] if not found. *)
val lookup : 'a -> ('a * 'b) list -> 'b option

(** Same, but no option type. Raises [Not_found] if it's not there. *) 
val lookup_failnull : 'a -> ('a * 'b) list -> 'b

(** [add k v l] Adds [(k, v)] to a dictionary, replacing any existing binding of
[k]. *)
val add : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** [replace k v l] replaces the existing binding of [k] in [l] with one with binds [k]
to [v]. Raises [Not_found] if there is nothing to replace. *)
val replace : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** Remove something from a list, if it's there. If not, don't complain. *)
val remove : 'a -> ('a * 'b) list -> ('a * 'b) list

(** Merge two lists, preferring elements in the second in the case of clashes. *)
val mergedict : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list

(** {2 Functions on references} *)

(** Set a boolean reference to [true] *)
val set : bool ref -> unit

(** Set a boolean reference to [false] *)
val clear : bool ref -> unit

(** Flip a boolean reference *)
val flip : bool ref -> unit

val ( += ) : int ref -> int -> unit
val ( -= ) : int ref -> int -> unit
val ( /= ) : int ref -> int -> unit
val ( *= ) : int ref -> int -> unit
(** Operations on integer references *)

val ( +.= ) : float ref -> float -> unit
val ( -.= ) : float ref -> float -> unit
val ( /.= ) : float ref -> float -> unit
val ( *.= ) : float ref -> float -> unit
(** Operations on floating point references *)

(** Cons something onto the contents of a list reference. *)
val ( =:: ) : 'a list ref -> 'a -> unit

(** Append something to the front of the contents of a list reference. *)
val ( =@ ) : 'a list ref -> 'a list -> unit

(** {2 Geometry} *)

(** The type of vectors. *)
type vector = float * float

(** [mkvector (a, b) (c, d)] makes a vector from point [(a, b)] to point [(c, d)]. *)
val mkvector : float * float -> float * float -> vector

(** Invert a vector. *)
val invert : vector -> vector

(** Offset a point by a vector. *)
val offset_point : vector -> float * float -> vector

(** Find the vector [pi / 2] anticlockwise from the given one. *)
val perpendicular : 'a * float -> float * 'a

(** The length of a vector. *)
val veclength : vector -> float

(** Scale a vector to a given length. *)
val scalevectolength : float -> vector -> vector

(** Make a unit vector in the direction from one point to a second. *)
val mkunitvector : float * float -> float * float -> vector

(** Find the point equidistant between two others. *)
val between : float * float -> float * float -> float * float

(** Cartesian distance between two points. *)
val distance_between : float * float -> float * float -> float

(** {2 Numbers} *)

(** Round a real. *)
val round : float -> float

(** The largest power of two by which a number is exactly divisible. *)
val largest_pow2_divisible : int -> int

(** Largest power of two smaller or equal to an integer. *)
val pow2lt : int -> int

(** Largest power of two greater or equal to an integer. *)
val pow2gt : int -> int

(** Base two logarithm *)
val log2of : int -> int

(** [pow x y] is y to the power x *)
val pow : int -> int -> int

(** Monomorphic integer version of [Pervasives.compare] *)
val compare_i : int -> int -> int

val min : int -> int -> int
val max : int -> int -> int
(** Monomorphic integer versions of [Pervasives] functions. *)

val fmin : float -> float -> float
val fmax : float -> float -> float
val fabs : float -> float
(** Monomorphic floating-point versions of [Pervasives] functions *)

val even : int -> bool
val odd : int -> bool
(** Even and odd predicates on integers. Work for negative numbers. *)

(** Pi. *)
val pi : float

(** Square root of two. *)
val root2 : float

val rad_of_deg : float -> float
val deg_of_rad : float -> float
(** Convert between radians and degrees. *)

(** {2 Options} *)

val some : 'a option -> bool
val none : 'a option -> bool
(** Predicates on the someness or noneness of an ['a option]. *)

(** Strip the [Some] from an option. Fail if it's [None]. *)
val unopt : 'a option -> 'a

(** {2 Miscellaneous functions} *)

(** The always-true predicate. *)
val always : 'a -> bool

(** The always-false predicate. *)
val never : 'a -> bool

(** Exclusive OR *)
val ( |&| ) : bool -> bool -> bool

(** The identity function. *)
val ident : 'a -> 'a

(** [iter2] on arrays. *)
val array_iter2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> unit

(** [map2] on arrays. *)
val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

(** [megabytes x] is [x * 1024 * 1024] *)
val megabytes : int -> int

(** Apply a function [f] [n] times to initial argument [arg]. *)
val applyn : ('a -> 'a) -> int -> 'a -> 'a

(** The type for binary trees. *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

(** The empty zero-sized hash table. *) 
val null_hash : unit -> ('a, 'b) Hashtbl.t

(** Make a list of key-value pairs reflecting the contents of a hash table. *)
val list_of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(** Build a hashtable from a dictionary (list of key-value pairs). Items are
added from left to right, with no checking for duplicate keys being performed. *)
val hashtable_of_dictionary : ('a * 'b) list -> ('a, 'b) Hashtbl.t

(** Make a tuple. *)
val tuple : 'a -> 'b -> 'a * 'b

(** [mkunit f x] gives [fun () -> f x] *)
val mkunit : ('a -> 'b) -> 'a -> unit -> 'b

(** [let ( & ) a b = a b] *)
val ( & ) : ('a -> 'b) -> 'a -> 'b

(** Consing to each of a pair of lists at the same time. *)
val conspair : ('a * 'b) * ('a list * 'b list) -> 'a list * 'b list

(** Version where there may or may not be somthing to cons in each case. *)
val conspairopt :
  ('a option * 'b option) * ('a list * 'b list) -> 'a list * 'b list

(** Make consecutive elements of an even-length list into a list of pairs. *)
val pairs_of_list : 'a list -> ('a * 'a) list

(** Make a list of the elements of a [Stream.t]. *)
val list_of_stream : 'a Stream.t -> 'a list


(** [until_exception r c] Perform computation [c] until an exception is raised,
with the dummy return value [r], of the type of the expression evaluted when the
exception is caught. *)
val until_exception : 'a -> (unit -> 'b) -> 'a

(** Set all the values of an array. *)
val set_array : 'a array -> 'a -> unit

(** [do_return f g] Evaluate [f ()], evaluate and ignore [g ()], return [f ()], in that order. *)
val do_return : (unit -> 'a) -> (unit -> 'b) -> 'a

(** [do_many f n] calls [f ()] n times. *)
val do_many : (unit -> 'a) -> int -> unit

(** A character is a decimal digit. *)
val isdigit : char -> bool

(** Same as [Pervasives.int_of_float] *)
val int : float -> int

(** Invert a predicate. *)
val notpred : ('a -> bool) -> 'a -> bool

(** Equality. *)
val eq : 'a -> 'a -> bool

(** Inequality. *)
val neq : 'a -> 'a -> bool

(** Like [Pervasives.compare], but the other way around. *)
val rev_compare : 'a -> 'a -> int

(** Print a string and flush standard output. *)
val flprint : string -> unit

(** Swaps the data at the given indexes in an array. *)
val swap : 'a array -> int -> int -> unit

(** Print a [Genlex.token] *)
val print_lexeme : Genlex.token -> unit

(**/**)

type stream = { mutable pos : int; mutable data : bytestream; }

val sqr : float -> float

val safe_float : float -> float

val box_union :
  int * int * int * int -> int * int * int * int -> int * int * int * int
 
val box_overlap :
  int -> int -> int -> int -> int -> int -> int -> int ->
    (int * int * int * int) option
val box_overlap_float :
  float -> float -> float -> float -> float -> float -> float -> float ->
  (float * float * float * float) option


