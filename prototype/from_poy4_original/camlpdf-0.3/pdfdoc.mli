(** Document-level functions *)

(** The type of the four rotations of pages. This defines how a viewing
application (e.g Acrobat) displays the page. *)
type rotation =
  Rotate0 | Rotate90 | Rotate180 | Rotate270

(** Utility function to convert from rotation to integers. *)
val int_of_rotation : rotation -> int

(** The reverse. raises [Pdf.PDFError] if its input modulo 360 is not 0, 90, 180
or 270. *)
val rotation_of_int : int -> rotation

(** A type representing a page. [content] is the list of objects containing the
graphical content stream (see the [Pdfpages] module), [mediabox] the page size,
[resources] the page's resource dictionary, [rotate] its rotation and [rest] any
other entries to reside in the page dictionary. *)
type page =
  {content : Pdf.pdfobject list;
   mediabox : Pdf.pdfobject;
   resources : Pdf.pdfobject;
   rotate : rotation;
   rest : Pdf.pdfobject}

(** Create a page with empty content, media box from the given paper size, empty
resources, zero rotation and no extra dictionary entries. *)
val blankpage : Paper.papersize -> page

(** Rename the resources within a number of page resource dictionaries and
contents, so as to allow them to be merged without name clashes. *)
val renumber_pages : Pdf.pdfdoc -> page list -> page list

(** Extract the page tree from a PDF document and parse it to a list of page
objects. Owing to the [rest] entry in the [page] type, no information is lost.
*)
val pages_of_pagetree : Pdf.pdfdoc -> page list

(** Build a page tree from a list of pages and install it in the given PDF
document. The resultant document and the number of the new page root object are
returned. If the document already contains a page root, it is overwritten but is
not garbage collected. *)
val add_pagetree : page list -> Pdf.pdfdoc -> Pdf.pdfdoc * int

(** Given the page root number (for instance that returned by [add_pagetree]),
any specific extra dictionary entries and a PDF document, build a document root.
Returns the new document. If a root exists, it is overwritten but is not garbage
collected. *)
val add_root : int -> (string * Pdf.pdfobject) list -> Pdf.pdfdoc -> Pdf.pdfdoc

(** Change the pages in a document for some new ones. If [change_references] is
true and the number of pages in the old and new documents are equal, references
to the old pages from outside the page tree (for instance in destinations or
bookmarks) are renumbered. This ensures bookmarks are preserved.*)
val change_pages : ?change_references : bool -> Pdf.pdfdoc -> page list -> Pdf.pdfdoc

(**/**)

(** Ensure that there are no inherited attributes in the page tree --- in other
words they are all explicit. This is required before writing a file with
linearization *)
val pagetree_make_explicit : Pdf.pdfdoc -> Pdf.pdfdoc

