type color = int
let black = 1

type display = {
    page_text : Pdfpages.operator list;
    current_file : string;
    contents : Pdf.pdfobject list;
    max_x : float;
    max_y : float;
    current_x : float;
    current_y : float;
    page_counter : int;
    cross_references : ((float * float) * int) All_sets.StringMap.t;
    pages : Pdfdoc.page list;
}

let add_reference display pos name =
    { display with cross_references =
        All_sets.StringMap.add name 
        (pos, display.page_counter) display.cross_references }

let toint x = Pdf.Real x

let make_link display name region =
    { display with contents =
        (Pdf.Array [Pdf.String "POYREF"; Pdf.String name;
        Pdf.Array (List.map toint region)]) :: display.contents }

let border = Pdf.Array (List.map toint [0.; 0.; 0.])

let fix_cross_references display pdf =
    let pages = Array.of_list (Pdf.page_reference_numbers pdf) in
    let make_destination = function
        | Pdf.Array [ Pdf.String "POYREF"; Pdf.String name; 
            (Pdf.Array _) as region] ->
                (* If a reference, then we replace it with the corresponding
                * destination *)
                (try
                    let (left, top), page = 
                        All_sets.StringMap.find name 
                        display.cross_references
                    in
                    let page = pages.(page) in
                    Pdf.Dictionary
                    [ "/Type", Pdf.Name "/Annot"; 
                    "/Subtype", Pdf.Name "/Link"; 
                    "/Rect", region;
                    "/Border", border;
                    "/A", Pdf.Dictionary [
                        "/S", Pdf.Name "/GoTo";
                        "/Dest", Pdf.Array [
                            Pdf.Indirect page;
                            Pdf.Name "/FitB"
                            (*
                            Pdf.Real left;
                            Pdf.Real top;
                            Pdf.Null*)]]]
                with
                | Not_found -> Pdf.Null)
        | obj -> 
                (* Now we need to split the annotations in the page objects, if
                * that's what we are visiting *)
                match Pdf.lookup_direct pdf "/Type" obj with
                | Some (Pdf.Name "/Page") ->
                        (* We are in a page, so we need to collect the 
                        * contents, and split those that really are 
                        * annotations *)
                        let new_contents, annotations =
                            match Pdf.lookup_direct pdf "/Contents" obj with
                            | Some (Pdf.Array lst) ->
                                    List.partition (fun obj ->
                                        let obj = Pdf.direct pdf obj in
                                        (match obj with
                                        | Pdf.Array 
                                            (Pdf.String "POYREF" :: _) ->
                                                false
                                        | _ -> true)) lst
                            | _ -> failwith "Malformed pdf?"
                        in
                        let obj =
                            Pdf.replace_dict_entry obj "/Contents" 
                            (Pdf.Array new_contents)
                        in
                        Pdf.add_dict_entry obj "/Annots" 
                        (Pdf.Array annotations)
                | Some _
                | None -> obj
    in
    Pdf.objmap make_destination pdf

let set_max display =
    { display with
        max_x = max display.max_x display.current_x;
        max_y = max display.max_y display.current_y }

let open_file (filename : string) = 
    {
        page_text = [];
        current_file = filename;
        contents = [];
        max_x = 0.;
        max_y = 0.;
        current_x = 0.;
        current_y = 0.;
        page_counter = 0;
        cross_references = All_sets.StringMap.empty;
        pages = [];
    }

let open_graph display size = display

let final_add_ops display ops =
    let ops = (Pdfpages.stream_of_ops ops) in
    Pdfcodec.encode_pdfstream Pdf.empty Pdfcodec.Flate ops;
    { display with contents =  ops :: display.contents }

let add_page display = 
    let display = final_add_ops display display.page_text in
    let page = 
        let font =
            Pdf.Dictionary
            [("/Type", Pdf.Name "/Font");
            ("/Subtype", Pdf.Name "/Type1");
            ("/BaseFont", Pdf.Name "/Times-Italic")]
        in
        let contents = display.contents in
        {(Pdfdoc.blankpage 
            (Units.PdfPoint, display.max_x, display.max_y)) with
            Pdfdoc.content = contents;
            resources = Pdf.Dictionary [("/Font", Pdf.Dictionary [("/F0",
            font)])]}
    in
    { display with pages = page :: display.pages; 
        page_text = [];
        page_counter = display.page_counter + 1;
        current_x = 0.;
        current_y = 0.;
        contents = [];
        max_x = 0.;
        max_y = 0. }

let close_graph display =
    let display = add_page display in
    let pdf = 
        let empty = 
            { Pdf.empty with Pdf.minor = 5 } 
        in
            let pdf, pageroot =
                Pdfdoc.add_pagetree (List.rev display.pages) empty
            in
            Pdfdoc.add_root pageroot [] pdf
    in
    let pdf = fix_cross_references display pdf in
    let () = Pdfwrite.pdf_to_file pdf display.current_file in
    ()


let add_ops display ops = 
    { display with page_text = ops @ display.page_text }

let text_size string = 
    (float_of_int (Fonttables.textwidth Pdftext.TimesItalic string)) /. 
    83., 6.

let draw_string ?tag ?link display string =
    if String.length string = 0 then display
    else
        let x, y = text_size string in
        let display =
            match tag with
            | None -> display
            | Some tag ->
                    add_reference display
                    (display.current_x, (display.current_y +.  y)) 
                    tag
        in
        let display =
            match link with
            | None -> display
            | Some link -> 
                    make_link display link 
                    [display.current_x; display.current_y; 
                    display.current_x +. x; display.current_y +. y]
        in
        let ops = 
            [ Pdfpages.Op_cm (Transform.matrix_of_transform 
                [Transform.Translate 
                    (display.current_x, display.current_y)]);
                Pdfpages.Op_BT; 
            Pdfpages.Op_Tf ("/F0", 12.);
            Pdfpages.Op_Tj string;
            Pdfpages.Op_ET;
            Pdfpages.Op_cm (Transform.matrix_of_transform [Transform.Translate
                        ((-. display.current_x), (-. display.current_y))]);

            ]
        in
        let display = { display with 
            current_x = display.current_x +. (x *. 2.);
            current_y = display.current_y +. (y *. 4.) }
        in
        let display = add_ops display ops in
        set_max display

let foreground = black

let lineto ?tag ?link display x y =
    let x = float_of_int x
    and y = float_of_int y in
    let display = 
        match tag with
        | None -> display 
        | Some tag ->
                add_reference display 
                (display.current_x, display.current_y) tag
    in
    let display =
        match link with
        | None -> display 
        | Some link ->
                make_link display link 
                [display.current_x; display.current_y; x; y ]
    in
    let ops = 
        [ 
            Pdfpages.Op_m (display.current_x, display.current_y);
            Pdfpages.Op_l (x, y);
            Pdfpages.Op_h;
            Pdfpages.Op_S
        ]
    in
    let display = add_ops display ops in
    set_max { display with current_x = x; current_y = y }

let polyline display lst =
    let current_x = ref display.current_x 
    and current_y = ref display.current_y in
    let ops = 
        match lst with
        | (a, b, tag, link) :: tl ->
                (Pdfpages.Op_m (float_of_int a, float_of_int b)) ::
                    (List.fold_right (fun (a, b, _, _) acc ->
                        let a, b = float_of_int a, float_of_int b in
                        current_x := a;
                        current_y := b;
                        (Pdfpages.Op_l (a, b)) :: acc) tl [Pdfpages.Op_S])
        | _ -> failwith "Polyline needs at least two points"
    in
    let display = 
        { display with current_x = !current_x; current_y = !current_y }
    in
    let display = add_ops display ops in
    set_max display

let move_to display x y =
    set_max { display with current_x = x; current_y = y } 

let plot display x y = 
    move_to display (float_of_int x) (float_of_int y)

let red = black
let set_color display _ = display
let size_x display = int_of_float display.max_x
let size_y display = int_of_float display.max_y

let display display = ()

let text_size string = 
    let a, b = text_size string in
    int_of_float a, int_of_float b

let moveto display a b =
    move_to display (float_of_int a) (float_of_int b)
