(* \chaptertitle{Pdfdraft}{Make Draft Documents} *)

(* Make a PDF suitable for draft printing by replacing its images by crossed
boxes. Summary: pdfdraft \texttt{input.pdf} \texttt{output.pdf}.*)
open Utility

(* Predicate on an xobject: true if an image xobject. *)
let isimage pdf (_, xobj) =
  Pdf.lookup_direct pdf "/Subtype" xobj = Some (Pdf.Name "/Image")

(* Given a set of resources for a page, and the name of a resource, determine if
that name refers to an image xobject. *)
let xobject_isimage pdf resources name =
  match resources with
  | Pdf.Dictionary _ ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some xobjects ->
          isimage pdf ("", Pdf.lookup_fail "xobject not there" pdf name xobjects)
      | _ -> false
      end
  | _ -> failwith "bad resources"

(* Remove any image xobjects from a set of resources. *)
let remove_image_xobjects pdf resources =
  match resources with
  | Pdf.Dictionary res ->
      begin match Pdf.lookup_direct pdf "/XObject" resources with
      | Some (Pdf.Dictionary xobjects) ->
          Pdf.Dictionary
            (replace "/XObject" (Pdf.Dictionary (lose (isimage pdf) xobjects)) res)
      | _ -> resources
      end
  | _ -> failwith "bad resources"

(* The subsitute for an image. *)
let substitute =
  rev
    [Pdfpages.Op_q;
     Pdfpages.Op_w 0.;
     Pdfpages.Op_G 0.;
     Pdfpages.Op_re (0., 0., 1., 1.);
     Pdfpages.Op_m (0., 0.);
     Pdfpages.Op_l (1., 1.);
     Pdfpages.Op_m (0., 1.);
     Pdfpages.Op_l (1., 0.);
     Pdfpages.Op_S;
     Pdfpages.Op_Q]

(* Remove references to images from a graphics stream. *)
let rec remove_images_stream pdf resources prev = function
  | [] -> rev prev
  | (Pdfpages.Op_Do name) as h::t ->
      if xobject_isimage pdf resources name
        then remove_images_stream pdf resources (substitute @ prev) t
        else remove_images_stream pdf resources (h::prev) t
  | Pdfpages.InlineImage _::t ->
      remove_images_stream pdf resources (substitute @ prev) t
  | h::t ->
      remove_images_stream pdf resources (h::prev) t

(* Remove images from a page. *)
let remove_images_page pdf page =
  let content' =
    remove_images_stream pdf page.Pdfdoc.resources []
       (Pdfpages.parse_operators pdf page.Pdfdoc.resources page.Pdfdoc.content)
  in
    {page with
      Pdfdoc.content =
        (let stream = Pdfpages.stream_of_ops content' in
          Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate stream;
          [stream]);
      Pdfdoc.resources =
        remove_image_xobjects pdf page.Pdfdoc.resources}

(* Remove images from all pages in a document. *)
let remove_images pdf =
  let pages = Pdfdoc.pages_of_pagetree pdf in
    let pages' = map (remove_images_page pdf) pages in
      let pdf, pagetree_num = Pdfdoc.add_pagetree pages' pdf in
        let pdf = Pdfdoc.add_root pagetree_num [] pdf in
          Pdf.remove_unreferenced pdf

(* Read command line arguments and call [remove_images] *)
let _ =
  match Array.to_list Sys.argv with
  | [_; in_file; out_file] ->
      begin try
        let ch = open_in_bin in_file in
          let pdf = Pdfread.pdf_of_channel ch in
            Pdfwrite.pdf_to_file (remove_images pdf) out_file;
            close_in ch
      with
        err ->
          Printf.printf "Failed to produce output.\n%s\n\n" (Printexc.to_string err);
          exit 1
      end
  | _ ->
      print_string "Syntax: pdfdraft <input> <output>\n\n"; exit 1

