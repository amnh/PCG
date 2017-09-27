(* \chaptertitle{Pdfdecomp}{Decompress streams} *)

(* Summary: \texttt{pdfdecomp a.pdf b.pdf} decompresses all streams in \texttt{a.pdf},
writing the result to \texttt{b.pdf}. *)

let decompress_pdf pdf =
  Pdf.map_stream (fun x -> Pdfcodec.decode_pdfstream_until_unknown pdf x; x) pdf

let _ =
  match Array.to_list Sys.argv with
  | [_; in_file; out_file] ->
      begin try
        let pdf = Pdfread.pdf_of_file in_file in
          Pdfwrite.pdf_to_file (decompress_pdf pdf) out_file
      with
        err ->
          Printf.printf "Failed to decompress file.\n%s\n\n" (Printexc.to_string err);
          exit 1
      end
  | _ ->
      print_string "Syntax: pdfdecomp <input> <output>\n\n"; exit 1
