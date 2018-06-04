let start_timer, stop_timer =
  let time = ref 0. in
  (fun () -> time := Unix.gettimeofday ()),
  (fun (msg:string) ->
    let delta = Unix.gettimeofday () -. !time in
    Printf.printf "  elapsed time for %s: %f s\n" msg delta;
    delta)
