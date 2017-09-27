(** 40-bit, 128-bit and AES Decryption *)

(** Note that encryption depends on fixed object and generation numbers: don't
change these (for example by calling [Pdf.remove_unreferenced] on the PDF)
before writing.

Encryption support is part of the [Pdfwrite] module.*)

(* Permissions. *)
type permission =
  | NoEdit 
  | NoPrint
  | NoCopy 
  | NoAnnot
  | NoForms
  | NoExtract
  | NoAssemble
  | NoHqPrint

(** Decrypt a PDF document, given the user password, returning the permissions
under which the document was encrypted. *)
val decrypt_pdf : string -> Pdf.pdfdoc -> Pdf.pdfdoc option * permission list

(** Decrypt a PDF document, given the owner password. *)
val decrypt_pdf_owner : string -> Pdf.pdfdoc -> Pdf.pdfdoc option

(**/**)
(* only for the use of PDFWrite *)

(** Encrypt a PDF documnent, using 40 bit encryption, with given user and
owner passwords. *)
val encrypt_pdf_40bit : string -> string -> permission list -> Pdf.pdfdoc -> Pdf.pdfdoc

(** Ditto for 128 bit encryption *)
val encrypt_pdf_128bit : string -> string -> permission list -> Pdf.pdfdoc -> Pdf.pdfdoc

(** Encrypt a file using the AESV2 Crypt filter *)
val encrypt_pdf_AES : bool -> string -> string -> permission list -> Pdf.pdfdoc -> Pdf.pdfdoc

(** Is a PDF encrypted? *)
val is_encrypted : Pdf.pdfdoc -> bool

(* only for the use of Pdfread *)
val decrypt_single_stream :
  string -> Pdf.pdfdoc -> int -> int -> Pdf.pdfobject -> Pdf.pdfobject

