(* \part{Examples} \chaptertitle{PdfHello}{Hello world, in PDF} *)

(* We build a font dictionary for one of the 14 standard PostScript fonts, (which
are supported by all PDF readers), make a graphics stream using the [Pdfpages]
module, build a PDF document in memory and then write it to \texttt{hello.pdf} *)
let _ =
  let font =
    Pdf.Dictionary
      [("/Type", Pdf.Name "/Font");
       ("/Subtype", Pdf.Name "/Type1");
       ("/BaseFont", Pdf.Name "/Times-Italic")]
  and ops =
    [Pdfpages.Op_cm (Transform.matrix_of_transform [Transform.Translate (50., 770.)]);
     Pdfpages.Op_BT;
     Pdfpages.Op_Tf ("/F0", 36.);
     Pdfpages.Op_Tj "Hello, World!";
     Pdfpages.Op_ET]
  in
    let page =
      {(Pdfdoc.blankpage Paper.a4) with
          Pdfdoc.content = [Pdfpages.stream_of_ops ops];
          Pdfdoc.resources = Pdf.Dictionary [("/Font", Pdf.Dictionary [("/F0", font)])]}
    in
      let pdf, pageroot = Pdfdoc.add_pagetree [page] Pdf.empty in
        let pdf = Pdfdoc.add_root pageroot [] pdf in
          Pdfwrite.pdf_to_file pdf "hello.pdf"

