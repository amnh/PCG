(* \chaptertitle{Pdftest}{Test on a document} *)

(* Summary: \texttt{pdftest in.pdf out.pdf} reads, lexes, parses a document
\texttt{in.pdf} and its graphics streams, then writes it to \texttt{out.pdf}.
*)
open Utility

let _ =
  let in_name, out_name =
    match tl (Array.to_list Sys.argv) with
    | [i; o] -> i, o
    | _ -> print_string "Syntax: pdftest <input> <output>\n\n"; exit 1
  in
    try
      let pdf = Pdfread.pdf_of_file in_name in
        let pages = Pdfdoc.pages_of_pagetree pdf in
          let pages' = 
            map 
              (fun page ->
                {page with Pdfdoc.content =
                  [Pdfpages.stream_of_ops
                    (Pdfpages.parse_operators pdf page.Pdfdoc.resources
                    page.Pdfdoc.content)]})
              pages
          in
            let pdf, pagetree_num = Pdfdoc.add_pagetree pages' pdf in
              let pdf = Pdfdoc.add_root pagetree_num [] pdf in
                Pdf.iter_stream (Pdfcodec.decode_pdfstream_until_unknown pdf) pdf;
                Pdf.iter_stream (Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate) pdf;
                Pdfwrite.pdf_to_file (Pdf.remove_unreferenced pdf) out_name
    with
      err ->
        Printf.printf "Test failed:\n%s\n\n" (Printexc.to_string err);
        exit 1

