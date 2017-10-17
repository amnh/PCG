let (-->) a b = b a

let merge_trees a b = 
    { a with Scripting.trees = Sexpr.union a.Scripting.trees b.Scripting.trees }

let rec run_with_static_approx_until_no_better run =
    let initial_cost = Phylo.Run.min_cost run in
    let new_run = (RPOY (run) swap (transform (static_approx))) in
    let final_cost = Phylo.Run.min_cost new_run in
    if initial_cost > final_cost then 
        run_with_static_approx_until_no_better new_run
    else run

let build_initial_tree run =
     run_with_static_approx_until_no_better (RPOY (run) build (1))

let build_more_trees trees run =
    (RPOY (run) build ([trees], transform (static_approx))), run

let improve_trees (to_improve, already_good) =
    let improved = Sexpr.fold_left (fun already_good x -> 
        { to_improve with Scripting.trees = `Single x }
        --> run_with_static_approx_until_no_better 
        --> merge_trees already_good) already_good to_improve.Scripting.trees 
    in
    (RPOY (improved) swap (constraint_p))

let do_fuse_round run =
    (RPOY (run) fuse ())

let fail () = 
    failwith "Usage: search2 ([(hours | minutes | seconds):FLOAT])"

let search_function args run =
    let time = 
        match args with
        | `Empty -> 3600. (* One hour *)
        | `Labled (name, `Float flt) ->
                if name = "hours" then
                    3600. *. flt
                else if name = "minutes" then
                    60. *. flt
                else if name = "seconds" then flt
                else fail ()
        | _ -> fail ()
    in
    let timer = Timer.start () in
    let rec do_iteration iteration run acc =
        if iteration = 0 || time < Timer.get_user timer then acc
        else
            run --> build_initial_tree --> build_more_trees 10
            --> improve_trees --> merge_trees acc 
            --> do_iteration (iteration - 1) run
    in
    let run = do_iteration 10 run run in
    do_fuse_round run

let () = Phylo.register_function "search2" search_function
