(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2015
*)

(*
  A soup is a partitionned set of abstract elements which is a candidate
  inductive invariant.

  We maintain extra information with each abstract element to allow fast
  computation of coverage and abstract operations (tightening, split, etc.)
 *)


open Bot
open Syntax
open Abstract_sig
open Parameters


(* unique identifier management *)

module Id = struct
  type t = int
  let next = ref 0
  let get () = incr next; !next
  let compare (a:t) (b:t) = compare a b
  let equal (a:t) (b:t) = a=b
  let hash (a:t) = a
end

module IdHash = Hashtbl.Make(Id)
module IdSet  = Set.Make(Id)
module IdMap  = Mapext.Make(Id)



(* main functor *)

module Soup(A:ABSTRACT) = struct

  module I = A.I
  module B = A.B

  type abs = A.t
  type id = Id.t

  module SVG = Svg.SVG(A)


  (* data structure *)
  (* ************** *)


  (*
    The space is partitioned into a set of parts.
    Each part can contain at most one abstract element.
    We also track the image of an element (which is a set of abstract
    elements), the parts overlapping each image bit, and which image
    bits overlap a part.


       [part] --- elem --> [elem] --- post --> [img] --- contained --> [part]
              <-- part ---        <-- org  ---       <-- covers    --- [part]

   *)


  type part = {
      pid: id;
      mutable space: abs;
      mutable elem: elem option;
      mutable covers: IdSet.t; (* images intersecting part *)
    }

  and elem = {
      part: part;
      mutable abs: abs;
      mutable post: IdSet.t;   (* f(abs) *)
      mutable coverage: float; (* coverage of post, in [0,1] (approximate) *)
      mutable covered: bool;   (* whether post is fully covered (exact) *)
      mutable locked: bool;    (* kept unchanged *)
    }

  and img = {
      iid: id;
      img: abs;
      org: id;                    (* element this image belongs to *)
      mutable contained: IdSet.t; (* parts intersecting image *)
    }


  (* priority queue *)

  module FMap = Mapext.Make(struct type t=float let compare=compare end)
  type pqueue = IdSet.t FMap.t


  (* soup state, holding all partitions, elements and images *)


  type soup = {
      f: (abs -> abs list);        (* transfer function *)
      init: abs;                   (* initial environments *)
      universe: abs;               (* bounds *)
      parts: part IdHash.t;        (* space partition *)
      elems: elem IdHash.t;        (* elements (same id as part) *)
      imgs: img IdHash.t;          (* images of elements *)
      mutable queue: pqueue;       (* elements to split *)

      mutable step: int;           (* count solving iteration *)

      svg: SVG.svg;                (* graphical output *)
    }




  (* output *)
  (* ****** *)


  let print_elem (soup:soup) ch (elem:elem) =
    let first = ref true in
    let print_img iid =
      let img = IdHash.find soup.imgs iid in
      Printf.fprintf ch "%s%a" (if !first then "" else "; ") A.output img.img;
      first := false
    in
    Printf.fprintf ch "%i: %a, in %a, img={"
                   elem.part.pid A.output elem.abs A.output elem.part.space;
    IdSet.iter print_img elem.post;
    let initial = A.intersect elem.abs soup.init in
    Printf.fprintf ch "}, coverage=%f%s, covers=%i, size=%f%s%s"
                   elem.coverage (if elem.covered then "(covered)" else "")
                   (IdSet.cardinal elem.part.covers)
                   (B.to_float_up (A.size elem.abs))
                   (if initial then " initial" else "")
                   (if elem.locked then " locked" else "")


  (* raw dump (for debugging) *)
  let dump ch (soup:soup) =
    IdHash.iter
      (fun pid part ->
        Printf.fprintf ch "part %i: space=%a elem=%B covers=" part.pid A.output part.space (part.elem<>None);
        IdSet.iter (fun iid -> Printf.fprintf ch "%i," iid) part.covers;
        Printf.fprintf ch "\n"
      ) soup.parts;
    IdHash.iter
      (fun pid elem ->
        Printf.fprintf ch "elem %i: abs=%a post=" elem.part.pid A.output elem.abs;
        IdSet.iter (fun iid -> Printf.fprintf ch "%i," iid) elem.post;
        Printf.fprintf ch " coverage=%f covered=%B\n" elem.coverage elem.covered;
      ) soup.elems;
    IdHash.iter
      (fun pid img ->
        Printf.fprintf ch "img %i: img=%a org=%i contained=" img.iid A.output img.img img.org;
        IdSet.iter (fun iid -> Printf.fprintf ch "%i," iid) img.contained;
        Printf.fprintf ch "\n"
      ) soup.imgs;
    FMap.iter
      (fun cover set ->
        IdSet.iter (fun id ->  Printf.fprintf ch "queue %f %i\n" cover id) set
      ) soup.queue




  let out_update_elem (soup:soup) (elem:elem) =
    if !svg_show_partition then
      SVG.change_elem soup.svg (-elem.part.pid-1) soup.step elem.part.space SVG.Part;
    SVG.change_elem soup.svg elem.part.pid soup.step elem.abs (SVG.Elem elem.coverage)

  let out_remove_elem (soup:soup) (elem:elem) =
    SVG.delete_elem soup.svg elem.part.pid soup.step




  (* internal functions *)
  (* ****************** *)


  (* priority queue managment *)

  let pqueue_add (soup:soup) (elem:elem) =
    if not elem.locked then
      let x = try FMap.find elem.coverage soup.queue with Not_found -> IdSet.empty in
      let x = IdSet.add elem.part.pid x in
      soup.queue <- FMap.add elem.coverage x soup.queue

  let pqueue_remove (soup:soup) (elem:elem) =
    let x = try FMap.find elem.coverage soup.queue with Not_found -> IdSet.empty in
    let x = IdSet.remove elem.part.pid x in
    soup.queue <-
      if x = IdSet.empty then FMap.remove elem.coverage soup.queue
      else FMap.add elem.coverage x soup.queue

  (* returns the highest priority element and removes it from queue *)
  let pqueue_pick (soup:soup) : elem =
    if FMap.is_empty soup.queue then invalid_arg "pqueue_pick: empty priority queue";
    let _,pids = FMap.min_binding soup.queue in
    assert(not (FMap.is_empty soup.queue));
    (* TODO: fair choice of elements with the same priority *)
    let pid = IdSet.min_elt pids in
    let elem = IdHash.find soup.elems pid in
    pqueue_remove soup elem;
    elem

  let pqueue_mem (soup:soup) (elem:elem) : bool =
    (FMap.mem elem.coverage soup.queue) &&
    (IdSet.mem elem.part.pid (FMap.find elem.coverage soup.queue))

  let pqueue_is_empty (soup:soup) : bool =
    FMap.is_empty soup.queue

  let pqueue_size (soup:soup) : int =
    FMap.fold (fun _ set acc -> acc + IdSet.cardinal set) soup.queue 0



  (* compute the thighest useful bound of an element;
     it encloses initial eleemnts as well as images of elements;
     the removed points do not participate in constructing an invariant *)
  let elem_bound (soup:soup) (elem:elem) : abs bot =
    IdSet.fold
      (fun iid abs ->
        let img = IdHash.find soup.imgs iid in
        A.join_bot abs (A.meet elem.abs img.img)
      )
      elem.part.covers
      (A.meet elem.abs soup.init)


  (* compute the coverage of an element;
     - exact coverage: fully covered (true) not fully covered (false)
     - approximate coverage, in [0,1] (not covered -> fully covered)
   *)
  let elem_coverage (soup:soup) (elem:elem) : bool * float =
    (* cover part for a bit of image *)
    let cover_img iid (covered,cover,vol) =
      let img = IdHash.find soup.imgs iid in
      let vol = B.add_down vol (A.volume_ext img.img) in
      let covered = covered && A.subseteq img.img soup.universe in
      (* \forall part: img.img \cap part.space \subseteq part.elem.abs *)
      IdSet.fold
        (fun pid (covered,cover,vol) ->
          let part = IdHash.find soup.parts pid in
          match part.elem with
          | None -> false, cover, vol
          | Some elem ->
              let inter = match A.meet part.space img.img with
              | Bot -> failwith "empty intersection in coverage"
              | Nb inter -> inter
              in
              covered && A.subseteq inter elem.abs,
              B.add_down cover (A.overlap inter elem.abs),
              vol
        )
        img.contained (covered,cover,vol)
    in
    (* accumulate over all images *)
    let covered, cover, vol = IdSet.fold cover_img elem.post (true,B.zero,B.zero)
    in
    (* normalize the coverage in [0,1] *)
    let coverage =
      if covered then 1.
      else max 0. (min 1. (B.to_float_down (B.div_down cover vol)))
    in
    covered, coverage


  (* returns the set of partition that covers elem's image;
     useful as argument for elem_update
   *)
  let elem_img_contained (soup:soup) (elem:elem) : IdSet.t =
    IdSet.fold
      (fun iid parts ->
        let img = IdHash.find soup.imgs iid in
        IdSet.union parts img.contained
      ) elem.post IdSet.empty


  (* removes an image, and update the coverage *)
  let img_remove (soup:soup) (iid:id) =
    if IdHash.mem soup.imgs iid then (
      let img = IdHash.find soup.imgs iid in
      IdHash.remove soup.imgs iid;
      IdSet.iter
        (fun pid ->
          let part = IdHash.find soup.parts pid in
          part.covers <- IdSet.remove iid part.covers
        ) img.contained
     )


  (* recomputes the coverage information of an element *)
  let elem_update_coverage (soup:soup) (elem:elem) =
    let covered, coverage = elem_coverage soup elem in
    pqueue_remove soup elem;
    elem.covered <- covered;
    elem.coverage <- coverage;
    if not covered then pqueue_add soup elem

  (* recomputes the coverage of all the elements with an image in this partition *)
  let part_update_coverage (soup:soup) (part:part) =
    let to_update =
      IdSet.fold
        (fun iid acc -> IdSet.add (IdHash.find soup.imgs iid).org acc)
        part.covers IdSet.empty
    in
    IdSet.iter
      (fun pid -> elem_update_coverage soup (IdHash.find soup.elems pid))
      to_update


  (* recomputes elem's image and the covering information;
     must be given a (conservative) set of parts that encompase the new image;
     when shrinking the element, "elem_img_contained soup elem" provides a
     good value for parts *)
  let elem_update (soup:soup) (elem:elem) (parts:IdSet.t) =
    (* remove old images *)
    IdSet.iter (img_remove soup) elem.post;
    (* create new images *)
    let mk_img acc img =
(*      assert(not (A.degenerate img));*)
      let iid = Id.get() in
      (* compute contained and updated covers for added images *)
      let contained =
        IdSet.filter
          (fun pid ->
            let part = IdHash.find soup.parts pid in
            let r = A.intersect img part.space in
            if r then part.covers <- IdSet.add iid part.covers;
            r
          ) parts
      in
      let img = { iid; img; org = elem.part.pid; contained; } in
      IdHash.add soup.imgs iid img;
      IdSet.add iid acc
    in
    let post = List.fold_left mk_img IdSet.empty (soup.f elem.abs) in
    (* update elem *)
    elem.post <- post;
    elem_update_coverage soup elem;
    (* update coverage for images covered by elem *)
    part_update_coverage soup elem.part;
    if !verbose_log then Printf.printf "update %a\n" (print_elem soup) elem;
    out_update_elem soup elem




  (* sanity checks *)
  (* ************* *)

  let validate (msg:string) (soup:soup) : unit =
    try
      assert(A.subseteq soup.init soup.universe);
      IdHash.iter
        (fun pid part ->
          assert(part.pid = pid);
          assert(List.length (IdHash.find_all soup.parts pid) = 1);
          assert(A.subseteq part.space soup.universe);
          assert((part.elem <> None) = (IdHash.mem soup.elems pid));
          assert(match part.elem with Some elem -> elem.part == part | None -> true);
          IdSet.iter
            (fun iid ->
              assert(IdHash.mem soup.imgs iid);
              let img = IdHash.find soup.imgs iid in
              assert(A.intersect part.space img.img);
              assert(IdSet.mem pid img.contained)
            ) part.covers
        ) soup.parts;
      IdHash.iter
        (fun pid elem ->
          assert(elem.part.pid = pid);
          assert(List.length (IdHash.find_all soup.elems pid) = 1);
          assert(IdHash.mem soup.parts pid);
          assert(A.subseteq elem.abs elem.part.space);
          assert(match elem.part.elem with None -> false | Some e -> e==elem);
          let post = soup.f elem.abs in
          assert(List.length post = IdSet.cardinal elem.post);
          IdSet.iter
            (fun iid ->
              assert(IdHash.mem soup.imgs iid);
              let img = IdHash.find soup.imgs iid in
              assert(img.org = pid);
              assert(List.exists (fun abs -> A.equal abs img.img) post);
            ) elem.post;
          let covered, coverage = elem_coverage soup elem in
          assert(elem.covered = covered);
          (*assert(elem.coverage = coverage);*) (* subject to float rounding? *)
          assert((not elem.covered && not elem.locked) = (pqueue_mem soup elem))
        ) soup.elems;
      IdHash.iter
        (fun iid img ->
          assert(img.iid = iid);
          assert(List.length (IdHash.find_all soup.imgs iid) = 1);
          assert(IdHash.mem soup.elems img.org);
          let elem = IdHash.find soup.elems img.org in
          assert(IdSet.mem iid elem.post);
          IdSet.iter
            (fun pid ->
              assert(IdHash.mem soup.parts pid);
              let part = IdHash.find soup.parts pid in
              assert(A.intersect img.img part.space);
              assert(IdSet.mem iid part.covers)
            ) img.contained
        ) soup.imgs;
      FMap.iter
        (fun cover pids ->
          assert(not (IdSet.is_empty pids));
          IdSet.iter
            (fun pid ->
              assert(IdHash.mem soup.elems pid);
              let elem = IdHash.find soup.elems pid in
              assert(not elem.covered);
              assert(elem.coverage = cover)
            ) pids;
        ) soup.queue
    with exn ->
      Printf.printf "validation failed for '%s'\n%a" msg dump soup;
      raise exn


  let validate_invariant (soup:soup) : unit =
    try
      (* includes init *)
      IdHash.iter
        (fun pid part ->
          match A.meet part.space soup.init, part.elem with
          | Nb init, Some elem -> assert(A.subseteq init elem.abs)
          | Nb _, None -> assert(false);
          | Bot, _ -> ()
        ) soup.parts;
      (* inductive *)
      IdHash.iter
        (fun pid elem ->
          IdSet.iter
            (fun iid ->
              let img = IdHash.find soup.imgs iid in
              assert(A.subseteq img.img soup.universe);
              (try
                let vol =
                  IdSet.fold
                    (fun pid' vol ->
                      assert(IdHash.mem soup.elems pid');
                      let part' = IdHash.find soup.parts pid'
                      and elem' = IdHash.find soup.elems pid' in
                      match A.meet img.img part'.space with
                      | Bot -> assert(false);
                      | Nb piece ->
                          assert(A.subseteq piece elem'.abs);
                          Q.add vol (A.exact_volume piece)
                    ) img.contained Q.zero
                in
                assert(Q.equal vol (A.exact_volume img.img))
              with Failure _ -> ())
            ) elem.post
        ) soup.elems
    with exn ->
      Printf.printf "inductive invariant validation failed\n%a" dump soup;
      raise exn



  (* main functions *)
  (* ************** *)


  (* create a new soup;
     it has a single partition (universe)
   *)
  let create (f:abs -> abs list) (init:abs) (universe:abs) : soup =
    assert(A.subseteq init universe);
    (* SVG output *)
    let x,y =
      if not (!svg_result || !svg_steps || !svg_animation) then "", "" else
      if !svg_x <> "" && !svg_y <> "" then !svg_x, !svg_y else
      match A.get_variables init with
      | xx::yy::_ -> xx,yy
      | _ -> "",""
    in
    let svg = SVG.create !svg_width !svg_height x y init universe in
    (* make soup, first with inexact image and coverage information *)
    let parts = IdHash.create 16
    and elems = IdHash.create 16
    and imgs = IdHash.create 16
    and pid = Id.get () in
    let part = { pid; space=universe; elem=None; covers=IdSet.empty; } in
    let elem = { part; abs=universe; post=IdSet.empty; coverage=(-1.); covered=false; locked=false; } in
    part.elem <- Some elem;
    IdHash.add parts pid part;
    IdHash.add elems pid elem;
    let queue = FMap.empty in
    let soup = { f; init; universe; parts; elems; imgs; queue; svg; step=0; } in
    (* compute actual image and coverage *)
    elem_update soup elem (IdSet.singleton pid);
    if !do_validate_step then validate "create" soup;
    soup


  (* remove an abstract element from the soup;
     the partition where elem lives becomes empty;
     updates the covering information
   *)
  let remove (soup:soup) (elem:elem) =
    (* remove part and image *)
    IdSet.iter (img_remove soup) elem.post;
    elem.part.elem <- None;
    IdHash.remove soup.elems elem.part.pid;
    pqueue_remove soup elem;
    (* update coverage *)
    part_update_coverage soup elem.part;
    if !do_validate_step then validate "remove" soup;
    out_remove_elem soup elem


  (* tighten an element to the smallest shape containing images *)
  let tighten (soup:soup) (elem:elem) =
    match elem_bound soup elem with
    | Bot -> remove soup elem
    | Nb abs when A.equal abs elem.abs -> ()
    | Nb abs ->
        elem.abs <- abs;
        elem_update soup elem (elem_img_contained soup elem)


  (* tighten elements and their images, etc. recursively until a fixpoint ;
     returns nb + the number of tightening/removal
   *)
  let rec tighten_fix (soup:soup) (elems:IdSet.t) nb =
    if IdSet.is_empty elems then nb else
    let pid = IdSet.min_elt elems in
    let elems = IdSet.remove pid elems in
    let elems,nb =
      if IdHash.mem soup.elems pid then
        let elem = IdHash.find soup.elems pid in
        match elem_bound soup elem with
        | Bot -> remove soup elem; elems, nb+1
        | Nb abs when A.equal abs elem.abs -> elems,nb
        | Nb abs ->
            elem.abs <- abs;
            elem_update soup elem (elem_img_contained soup elem);
            IdSet.union elems (elem_img_contained soup elem), nb+1
      else elems, nb
    in
    tighten_fix soup elems nb



  (* removes an element and tigheten recursively *)
  let remove_and_tighten (soup:soup) (elem:elem) =
    let to_tighten = elem_img_contained soup elem in
    remove soup elem;
    ignore (tighten_fix soup (IdSet.remove elem.part.pid to_tighten) 0)



  (* tighten all elements, recursively, until a fixpoint is reached *)
  let tighten_all (soup:soup) =
    let all = IdHash.fold (fun pid _ acc -> IdSet.add pid acc) soup.elems IdSet.empty in
    let nb = tighten_fix soup all 0 in
    Printf.printf "  - full tightening: %i element(s) tightened\n" nb;
    if !do_validate_step then validate "tighten_all" soup



  (* split the given element and refine the partition
   *)
  let split (soup:soup) (elem:elem) =
    let var,range = A.max_range elem.abs in
    if B.equal (fst range) (snd range) then
      (* cannot split anymore *)
      elem.locked <- true
    else
      let mean = I.mean range in
      match A.split elem.abs var mean, A.split elem.part.space var mean with
      | (Nb e1, Nb e2), (Nb p1, Nb p2) ->
          (* cut part *)
          let part1 = elem.part
          and part2 = { elem.part with pid = Id.get(); } in
          part1.space <- p1;
          part2.space <- p2;
          IdHash.add soup.parts part2.pid part2;
          (* update part in contained *)
          let updt_img (part:part) (iid:id) =
            let img = IdHash.find soup.imgs iid in
            let f =
              if A.intersect img.img part.space
              then IdSet.add
              else IdSet.remove
            in
            img.contained <- f part.pid img.contained;
            part.covers <- f img.iid part.covers
          in
          IdSet.iter (updt_img part1) part1.covers;
          IdSet.iter (updt_img part2) part2.covers;
          (* convervative overlap information for element after cut *)
          let parts = elem_img_contained soup elem in
          let parts =
            if IdSet.mem part1.pid parts then IdSet.add part2.pid parts else parts
          in
          (* cut element *)
          elem.abs <- e1;
          let elem2 = { elem with abs = e2; part = part2; coverage = -1.; } in
          part2.elem <- Some elem2;
          IdHash.add soup.elems part2.pid elem2;
          elem_update soup elem  parts;
          elem_update soup elem2 parts;

          if !aggressive_tightening then
            (* recursive tighetening *)
            ignore (tighten_fix soup
                      (IdSet.add elem2.part.pid (IdSet.singleton elem.part.pid)) 0)
          else (
            (* tighten only the split elements, not the images they cover *)
            tighten soup elem;
            tighten soup elem2
           );

          if !do_validate_step then validate "split" soup

      | _ -> failwith "TODO: cannot cut"




  (* remove unreachable from init *)
  let remove_unreachable (soup:soup) =
    (* depth-first search *)
    let rec doit pid acc =
      if IdSet.mem pid acc || not (IdHash.mem soup.elems pid) then acc else
      let acc = IdSet.add pid acc in
      let elem = IdHash.find soup.elems pid in
      IdSet.fold doit (elem_img_contained soup elem) acc
    in
    (* get reachable from init *)
    let reachable,all =
      IdHash.fold
        (fun id elem (acc,all) ->
          (if A.intersect elem.abs soup.init then doit id acc else acc),
          (IdSet.add id all)
        ) soup.elems (IdSet.empty,IdSet.empty)
    in
    Printf.printf "  - %i reachable, %i total\n"
      (IdSet.cardinal reachable) (IdSet.cardinal all);
    IdSet.iter
      (fun pid -> remove soup (IdHash.find soup.elems pid))
      (IdSet.diff all reachable);
    if !do_validate_step then validate "remove_unreachable" soup



  (* remove elements that are reachable after more than max-nb application
     of post from init, where max is the maximum number of post to reach all
     the reachable elements in the soup.
   *)
  let remove_fringe (soup:soup) (nb:int) =
    (* breath-first search from init *)
    let rec doit level circle all =
      if IdSet.is_empty circle then all else
      let all = IdSet.fold (fun pid all -> IdMap.add pid level all) circle all in
      let circle' =
        IdSet.fold
          (fun pid circle' ->
            if not (IdHash.mem soup.elems pid) then circle' else
            let elem = IdHash.find soup.elems pid in
            let post = elem_img_contained soup elem in
            IdSet.union circle' (IdSet.filter (fun pid -> not (IdMap.mem pid all)) post)
          ) circle IdSet.empty
      in
      doit (level+1) circle' all
    in
    (* init *)
    let level0 =
      IdHash.fold
        (fun pid elem level0 ->
          if A.intersect elem.abs soup.init then IdSet.add pid level0
          else level0
        ) soup.elems IdSet.empty
    in
    (* launch search and get the level of every element *)
    let all = doit 0 level0 IdMap.empty in
    (* removal *)
    let max_level = IdMap.fold (fun _ level acc -> max level acc) all 0 in
    let threshold = max 1 (max_level - nb) in
    let nb_elem = ref 0 in
    IdMap.iter
      (fun pid level ->
        if level >= threshold && IdHash.mem soup.elems pid
        then (incr nb_elem; remove soup (IdHash.find soup.elems pid))
      ) all;
    Printf.printf "  - remove fringe %i, max level %i: %i element(s) removed\n"
      nb max_level !nb_elem;
    if !do_validate_step then validate "remove_fringe" soup



  (* extra refinement;
     we resplit to avoid the image of an element to spread across too many partitions
   *)
  let resplit (soup:soup) (threshold:int) =
    let toresplit =
      IdHash.fold
        (fun _ img acc ->
          if IdSet.cardinal img.contained < threshold then acc
          else IdSet.add img.org acc
        ) soup.imgs IdSet.empty
    in
    Printf.printf "  - resplit with threshold %i: %i element(s) resplit\n"
      threshold (IdSet.cardinal toresplit);
    IdSet.iter
      (fun pid ->
        if IdHash.mem soup.elems pid
        then split soup (IdHash.find soup.elems pid)
      ) toresplit;
    if !do_validate_step then validate "resplit" soup



  (* number of elements covered and not covered in soup *)
  let coverage_stat (soup:soup) : int * int =
    IdHash.fold
      (fun pid elem (n1,n2) -> if elem.covered then n1+1, n2 else n1, n2+1)
      soup.elems (0,0)


  let time = ref (Unix.gettimeofday ())
  let start_timer () =
    time := Unix.gettimeofday ()
  let stop_timer (msg:string) =
    let delta = Unix.gettimeofday () -. !time in
    Printf.printf "  elapsed time for %s: %f s\n" msg delta;
    delta



  (* solver *)
  (* ****** *)

  let solve
        (fwd:abs -> abs list)
        (bwd:abs list -> abs)
        (init:abs)
        (universe:abs) : unit =

    let epsilon_size = ref (B.div_down (B.of_float_down !epsilon_size) (A.size init))
    and epsilon_cover = ref !epsilon_cover in

    Printf.printf "init: %a\nuniverse: %a\n%!" A.output init A.output universe;

    (* create the initial soup *)
    let soup = create fwd init universe in

    (* iteration loop *)
    let rec iterate step i =
      soup.step <- soup.step + 1;
      if i > !max_iterate then
        (* exit due to maximal iteration count reached *)
        Printf.printf "  maximum iteration reached\n"
      else if pqueue_is_empty soup then
        (* exit as nothing more to do *)
        Printf.printf "  %i iteration(s)\n" i
      else
        (* find the element to handle *)
        let elem = pqueue_pick soup in
        if !verbose_log then
          Printf.printf "iteration %i, %i elem(s), select %a"
                        i ((pqueue_size soup) + 1) (print_elem soup) elem;
        (* handle the element *)
        let small = B.lt (A.size elem.abs) !epsilon_size in
        let uncovered = elem.coverage <= !epsilon_cover in
        let initial = A.intersect elem.abs init in
        let useless = IdSet.is_empty elem.part.covers in
        if !verbose_log then
          Printf.printf " small=%B uncovered=%B initial=%B useless=%B"
                        small uncovered initial useless;
        if (uncovered || small || useless) && not initial then (
          (* (too small or not enough covered) and not initial => remove *)
          if !verbose_log then Printf.printf " => removed\n%!";
          if !aggressive_tightening then remove_and_tighten soup elem
          else remove soup elem
        )
        else if small then (
          (* too small but initial => keep intact *)
          if !verbose_log then Printf.printf " => kept\n%!";
          elem.locked <- true
        )
        else (
          (* not too small, good enough coverage => split *)
          if !verbose_log then Printf.printf " => split\n%!";
          split soup elem
         );

        let m =
          if i <= 10 then 1
          else if i <= 100 then 10
          else if i <= 1000 then 100
          else if i <= 10000 then 1000
          else 10000
        in
        if !svg_steps && i mod m = 0 then
          SVG.save_image (Printf.sprintf "%s%s-%010i.svg" !svg_prefix step i) soup.svg;

        (* next iteration *)
        iterate step (i+1)
    in

    (* perform iterations & clean the result *)
    let do_iteration step =
      Printf.printf "  fixpoint search step %s\n" step;
      Printf.printf "  epsilon_size: %f, epsilon_cover: %f\n"
        (B.to_float_down !epsilon_size) !epsilon_cover;
      flush stdout;

      iterate step 1;

      soup.step <- soup.step + 1;
      remove_unreachable soup;
      soup.step <- soup.step + 1;
      tighten_all soup;
      soup.step <- soup.step + 1;
      remove_unreachable soup;

      let covered, noncovered = coverage_stat soup in
      Printf.printf "  %i ELEMS: covered = %i, noncovered = %i\n" (covered+noncovered) covered noncovered;
      if noncovered = 0 then Printf.printf "  => INDUCTIVE INVARIANT FOUND\n%!"
      else Printf.printf "  => FAILED\n%!";
      if !svg_result then SVG.save_image (Printf.sprintf "%s%s-result%s.svg" !svg_prefix step (if noncovered = 0 then "-ok" else "")) soup.svg;
    in

    (* first, find an inductive invariant *)
    let first_success = ref (-1) in
    let last_success = ref (-1) in
    start_timer ();
    do_iteration "0";
    let total_time = ref (stop_timer "step 0") in
    let success_time = ref !total_time in

    (* then, refine the inductive invariant *)
    let rec refine_loop step =
      epsilon_size := B.mul_up !epsilon_size (B.of_float_up !epsilon_falloff);
      epsilon_cover := !epsilon_cover *. !epsilon_falloff;
      let covered, noncovered = coverage_stat soup in
      if !do_validate_final then validate "final" soup;
      if !do_validate_final && noncovered = 0 then validate_invariant soup;
      if noncovered = 0 then (
        last_success := step - 1;
        if !first_success < 0 then first_success := !last_success
       );
      let max_steps =
        if !last_success >= 0 then !max_refinement_steps else !max_search_steps
      in
      if step <= max_steps then (
        start_timer ();
        if noncovered = 0 then (
          (* success: try refining the result *)
          Printf.printf "Refinement iteration %i\n" step;
          flush stdout;

          for i=1 to !resplit_steps do
            soup.step <- soup.step + 1;
            resplit soup !resplit_threshold
          done;

          if !fringe_size > 0 then (
            soup.step <- soup.step + 1;
            remove_fringe soup !fringe_size
           );

          soup.step <- soup.step + 1;
          remove_unreachable soup;
          soup.step <- soup.step + 1;
          tighten_all soup;

          do_iteration (string_of_int step);
         )
        else (
          (* failure: try again after split *)
          Printf.printf "Retry iteration %i\n" step;
          flush stdout;

          for i=1 to !resplit_failed_steps do
            soup.step <- soup.step + 1;
            resplit soup !resplit_failed_threshold
          done;

          soup.step <- soup.step + 1;
          remove_unreachable soup;
          soup.step <- soup.step + 1;
          tighten_all soup;

          do_iteration (string_of_int step);
         );
        let time = stop_timer (Printf.sprintf "step %i" step) in
        if !last_success < 0 then
          success_time := !success_time +. time;
        total_time := !total_time +. time;
        refine_loop (step+1)
       )
    in
    refine_loop 1;


    (* check and print result *)

    if !last_success >= 0 then
      Printf.printf "\nSUCCESS, first at step %i, last at step %i\nTime to first success: %f s\nTotal time: %f s\n" !first_success !last_success !success_time !total_time
    else
      Printf.printf "\nFAILURE after %i step(s)\nTotal time : %f s\n" !max_search_steps !total_time;

    if !svg_result then SVG.save_image (Printf.sprintf "%sresult.svg" !svg_prefix) soup.svg;
    if !svg_animation then SVG.save_animation (Printf.sprintf "%sanimation.svg" !svg_prefix) soup.svg

end
