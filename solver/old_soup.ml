(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2015
*)

(*
  A soup is a set of boxes which is a candidate inductive invariant.
  
  We maintain extra information with each box to allow fast computation
  of coverage and abstract operations (tightening, split, etc.)
 *)


open Bot
open Syntax
open Abstract_sig
open Parameters



(* Functor *)
(* ******* *)
    
module Soup(A:ABSTRACT) = struct
  
  module I = A.I
  module B = I.B
  module Svg = Svg.SVG(A)
    

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)


  type box = A.t

  (* unique element identifier *)
  type id = int


  (* a soup element is a box, with extra information;
     elements are immutable
   *)
  type elem = 
      { id: id;         (* unique identifier *)
        box: box;       (* geometrical interpretation *)
        image: box;     (* image of box by f *)
        post: id list;  (* id of elements in soup intersecting image *)
        pre: id list;   (* id of elements in soup whose image intersect box *)
        cover: B.t;     (* how much (in [0,1]) of the image is included in the soup *)
        initial: bool;  (* whether the box intersects the initial one *)
      }


  (* set of elements sorted by increasing coverage, with a bias:
    - to allow old elements to be cut from time to time
      - to distinguish elements with identical weight
   *)
  module FSet = Set.Make 
      (struct 
        type t = B.t * id 
        let w x i = B.add_up x (B.div_up (B.of_int_up i) (B.of_int_down 10000))
        let compare (x,i) (y,j) = 
        let x = B.compare (w x i) (w y j) in
        if x <> 0 then x else compare i j
      end)
      

  (* soup;
     the soup is mutable, it corresponds to the current state of the solver
     and it is updated by solver operations
   *)
  type soup =
      { 
        (* the function whose fixpoint we are computing *)
        f: (box -> box); 
        
        (* initial states, the fixpoint will include this box *)
        init: box;
        
        (* goal invariant *)
        goal: box;
        
        (* all the elements in the soup *)
        elems: (id,elem) Hashtbl.t;
        
        (* the element, sorted by the prefered order of refinement *)
        (* elements with coverage 1 are not present here *)
        mutable covers: FSet.t;
        
        (* graphical output *)
        out: Svg.svg;
        mutable time: Svg.time;
      }



  (************************************************************************)
  (* UTILITIES *)
  (************************************************************************)


  (* compute the coverage of given box image, covered with post in soup *)
  let coverage (soup:soup) (image:box) (post:id list) : B.t =
    B.div_up
      (List.fold_left
         (fun acc id' ->
           let e' = Hashtbl.find soup.elems id' in
           B.add_up acc (A.overlap image e'.box)
         ) 
         B.zero post
      )
      (A.volume image)
    



  (* check the the soup data-structure is valid;
     used only for debugging
   *)
  let validate (soup:soup) =
    assert (A.subseteq soup.init soup.goal);
    Hashtbl.iter
      (fun id e ->
        assert (e.id = id);
        assert (A.subseteq e.box soup.goal);
        assert (A.equal e.image (soup.f e.box));
(*        assert (B.equal e.cover (coverage soup e.image e.post));*)
        assert (e.initial = A.intersect e.box soup.init);
        List.iter (fun id' -> assert (Hashtbl.mem soup.elems id')) e.pre;
        List.iter (fun id' -> assert (Hashtbl.mem soup.elems id')) e.post;
        Hashtbl.iter
          (fun id' e' ->
            if id = id' then () else
            assert (B.sign (A.overlap e.box e'.box) = 0);
            let inter = A.intersect e.image e'.box in
            assert (inter = List.exists ((=) id') e.post);
            assert (inter = List.exists ((=) id) e'.pre)
          ) soup.elems;
      ) soup.elems;
    FSet.iter
      (fun (cover,id) ->
        assert (Hashtbl.mem soup.elems id);
        let e = Hashtbl.find soup.elems id in
        assert (B.equal cover e.cover)
      ) soup.covers



  (* creates new identifiers *)
  let new_id : unit -> id =
    let id = ref 0 in
    fun () -> incr id; assert (!id > 0); !id
        

  let rec print_int_list c = function
    | [] -> Printf.fprintf c "()"
    | [a] -> Printf.fprintf c "%i" a
    | a::l -> Printf.fprintf c "%i,%a" a print_int_list l

  let print_int_list_length c l =
    Printf.printf "%i" (List.length l)
      
      
  (* prints element (for debugging) *)
  let print_elem c e =
    Printf.fprintf c "%i: %a, image %a cover %f, pre %a, post %a%s" 
      e.id A.output e.box A.output e.image (B.to_float_up e.cover) 
      print_int_list_length e.pre print_int_list_length e.post 
      (if e.initial then ", initial" else "")



  (************************************************************************)
  (* SOUP OPERATIONS *)
  (************************************************************************)


  (* create a new soup composed of a single box (the goal) *)
  let create_soup 
      (f:box -> box) 
      (init:box) (goal:box) : soup = 
    
    (* setup output *)
    let x,y = 
      if not (!svg_result || !svg_steps || !svg_animation) then "", "" else
      if !svg_x <> "" && !svg_y <> "" then !svg_x, !svg_y else
      match A.get_variables init with
      | xx::yy::_ -> xx,yy
      | _ -> "",""
    in
    let out = Svg.create !svg_width !svg_height x y init goal in

    (* setup soup *)
    let id = new_id () in
    let img = f goal in
    let cover = B.div_up (A.overlap goal img) (A.volume img) in
    assert (B.sign cover > 0);
    let elem = 
      { id = id; box = goal; image = img; cover = cover; initial = true;
        post = [id]; pre = [id];
      } 
    in
    let soup = 
      { elems = Hashtbl.create 10; 
        covers = FSet.singleton (cover,id);
        goal; init; f;
        out; time=0;
      }
    in
    Hashtbl.add soup.elems id elem;
    Svg.add_elem soup.out id soup.time elem.box (Svg.Elem (B.to_float_down elem.cover));
    if !do_validate_final then validate soup;
    soup
      

  (* removes the element from the queue, but keep it in the soup *)
  let lock_elem (soup:soup) (id:id) : unit =
    assert (Hashtbl.mem soup.elems id);
    let e = Hashtbl.find soup.elems id in
    soup.covers <- FSet.remove (e.cover,e.id) soup.covers


  (* remove the element id from the soup *)
  let remove_elem (soup:soup) (id:id) : unit =
    assert (Hashtbl.mem soup.elems id);
    let e = Hashtbl.find soup.elems id in
    (* update post and cover *)
    List.iter
      (fun id' ->
        if id = id' then () else
        let e' = Hashtbl.find soup.elems id' in
        let post' = List.filter ((<>) id) e'.post in
        let cover' = coverage soup e'.image post' in
        Hashtbl.replace soup.elems id' { e' with post = post'; cover = cover'; };
        Svg.change_elem soup.out id' soup.time e'.box (Svg.Elem (B.to_float_down cover'));
        soup.covers <- FSet.remove (e'.cover,id') soup.covers;
        if B.lt cover' B.one then soup.covers <- FSet.add (cover',id') soup.covers;
      ) e.pre;
    (* update pre *)
    List.iter
      (fun id' ->
        if id = id' then () else
        let e' = Hashtbl.find soup.elems id' in
        let pre' = List.filter ((<>) id) e'.pre in
        Hashtbl.replace soup.elems id' { e' with pre = pre'; }
      ) e.post;
    (* update soup *)
    Hashtbl.remove soup.elems id;
    soup.covers <- FSet.remove (e.cover,id) soup.covers;
    Svg.delete_elem soup.out id soup.time;
    if !do_validate_step then validate soup


  (* shrink an elemen to enclose the image of pre more tightly;
     we could use a fixpoint to shrink even more
   *)
  let shrink_elem (soup:soup) (id:id) : unit =
    assert (Hashtbl.mem soup.elems id);
    let e = Hashtbl.find soup.elems id in
    (* join with intersections with image of pre *)
    let b = 
      List.fold_left
        (fun cur id'' ->
          let e'' = Hashtbl.find soup.elems id'' in
          A.join_bot cur (A.meet e.box e''.image)
        )
        Bot e.pre
    in
    (* also join with intersection with initial *)
    let b =
      if e.initial then A.join_bot b (A.meet e.box soup.init)
      else b
    in
    match b with
    | Nb b ->
        if A.equal e.box b then () (* no update *) else
        (* update element *)
        let e' = 
          { e with
            box = b; 
            image = soup.f b;
            initial = A.intersect b soup.init; 
          }
        in
        Hashtbl.replace soup.elems id e';
        let post =
          List.filter
            (fun id'' ->
              let e'' = Hashtbl.find soup.elems id'' in
              A.intersect e'.image e''.box
            ) e.post
        in
        let cover = coverage soup e'.image post in
        let e' = { e' with post = post; cover = cover; } in
        Hashtbl.replace soup.elems id e';
        Svg.change_elem soup.out id soup.time e'.box (Svg.Elem (B.to_float_down e'.cover));
        soup.covers <- FSet.remove (e.cover,id) soup.covers;
        if B.lt e'.cover B.one
        then soup.covers <- FSet.add (e'.cover,id) soup.covers;
        (* update post of other elements *)
        List.iter 
          (fun id'' ->
            if not (List.exists ((=) id'') post) then
              let e'' = Hashtbl.find soup.elems id'' in
              let pre = List.filter ((<>) id) e''.pre in
              Hashtbl.replace soup.elems id'' { e'' with pre = pre; }
          )
          e.post;
        if !verbose_log then 
          Printf.printf "  shrink %a -> %a\n" print_elem e print_elem e';
        if !do_validate_step then validate soup
    | _ ->
        remove_elem soup id
          


  (* split the element id in the soup;
     the two sub-boxes are then shrunk
   *)
  let split_elem (soup:soup) (id:id) : unit =
    assert (Hashtbl.mem soup.elems id);
    let e = Hashtbl.find soup.elems id in
    (* split element e into two new elements *)
    let var,range = A.max_range e.box in
    if I.is_singleton range then
      (* cannot split anymore *)
      lock_elem soup id
    else match A.split e.box var (I.mean range) with
    | Nb b1, Nb b2 ->
        (* succesful split *)
        let dup b = 
          { e with 
            id = new_id(); 
            box = b; 
            image = soup.f b;
            initial = A.intersect b soup.init; 
          } in
        let e1,e2 = dup b1, dup b2 in
        Hashtbl.remove soup.elems id;
        soup.covers <- FSet.remove (e.cover,id) soup.covers;
        Hashtbl.add soup.elems e1.id e1;
        Hashtbl.add soup.elems e2.id e2;
        (* replace occurences of id with that of new elems *)
        let updt l = 
          if List.exists ((=) id) l then [e1.id;e2.id]@(List.filter ((<>) id) l)
          else l
        in
        (* compute pre, post, cover of the two new elements *)
        (* new post and pre are subsets of the old ones *)
        let post e =
          List.filter
            (fun id' -> 
              let e' = Hashtbl.find soup.elems id' in
              A.intersect e.image e'.box
            ) (updt e.post)
        and pre e =
          List.filter
            (fun id' ->
              let e' = Hashtbl.find soup.elems id' in
              A.intersect e'.image e.box
            ) (updt e.pre)
        in
        let post1, post2 = post e1, post e2 in
        let pre1, pre2 = pre e1, pre e2 in
        let cover1 = coverage soup e1.image post1
        and cover2 = coverage soup e2.image post2 in
        let e1 = { e1 with pre = pre1; post = post1; cover = cover1; }
        and e2 = { e2 with pre = pre2; post = post2; cover = cover2; } in
        Hashtbl.replace soup.elems e1.id e1;
        Hashtbl.replace soup.elems e2.id e2;
        Svg.delete_elem soup.out id soup.time;
        Svg.add_elem soup.out e1.id soup.time e1.box (Svg.Elem (B.to_float_down e1.cover));
        Svg.add_elem soup.out e2.id soup.time e2.box (Svg.Elem (B.to_float_down e2.cover));
        if B.lt e1.cover B.one
        then soup.covers <- FSet.add (e1.cover,e1.id) soup.covers;
        if B.lt e2.cover B.one
        then soup.covers <- FSet.add (e2.cover,e2.id) soup.covers;
        (* update pre / post of other elements *)
        List.iter
          (fun id' ->
            try
              let e' = Hashtbl.find soup.elems id' in
            Hashtbl.replace soup.elems id' { e' with pre = pre e'; }
            with Not_found -> ()
          ) e.post;
        List.iter
          (fun id' ->
            try
              let e' = Hashtbl.find soup.elems id' in
              Hashtbl.replace soup.elems id' { e' with post = post e'; }
            with Not_found -> ()
          ) e.pre;
        if !do_validate_step then validate soup;
        shrink_elem soup e1.id;
        shrink_elem soup e2.id
    | _ ->
        (* cannot split anymore *)
        lock_elem soup id



  (************************************************************************)
  (* SOLVER *)
  (************************************************************************)


  let solve 
      (f:box -> box) 
      (init:box) (goal:box) : unit =

    let epsilon_size = B.div_up (B.of_float_up !epsilon_size) (A.volume init)
    and epsilon_cover = B.of_float_up !epsilon_cover
    in

    Printf.printf "init: %a\ngoal: %a\n%!" A.output init A.output goal;

    (* create the initial soup *)
    let soup = create_soup f init goal in
    
    (* iteration loop *)
    let rec iterate i = 

      soup.time <- i;
      if i > !max_iterate then 
        (* exit due to maximal iteration count reached *)
        Printf.printf "maximum iteration reached\n"
          
      else if FSet.is_empty soup.covers then 
        (* exit as nothing more to do *)
        Printf.printf "%i iteration\n" i
          
      else
        (* find the element to handle *)
        let cover,id = FSet.min_elt soup.covers in
        let e = Hashtbl.find soup.elems id in
        if !verbose_log then 
          Printf.printf "iteration %i, %i elem(s), select %a" 
            i (FSet.cardinal soup.covers) print_elem e;
        
        (* handle the element *)
        let small = B.leq (A.size e.box) epsilon_size in
        if (B.leq cover epsilon_cover || small || e.pre = []) && not e.initial then (
          (* (too small or not enough covered) and not initial => remove *)
          if !verbose_log then Printf.printf " => removed\n%!"; 
          remove_elem soup id
         )
        else if small then (
          (* too small but initial => keep intact *)
          if !verbose_log then Printf.printf " => kept\n%!"; 
          lock_elem soup id
         )
        else (
          (* not too small, good enough coverage => split *)
          if !verbose_log then Printf.printf " => split\n%!"; 
          split_elem soup id
         );
        
        let m = 
          if i <= 10 then 1 
          else if i <= 100 then 10
          else if i <= 1000 then 100
          else if i <= 100000 then 1000
          else 10000
        in
        if !svg_steps && i mod m = 0 then
          Svg.save_image (Printf.sprintf "%s%010i.svg" !svg_prefix i) soup.out;
        
        (* next iteration *)
        iterate (i+1)
    in 
    
    (* launch iterations *)
    iterate 1;
    if !svg_result then
      Svg.save_image (Printf.sprintf "%sresult.svg" !svg_prefix) soup.out;
    if !svg_animation then
      Svg.save_animation (Printf.sprintf "%sanimation.svg" !svg_prefix) soup.out;
    
    (* check result *)
    let noncovered, covered =
      Hashtbl.fold
        (fun _ e (n1,n2) ->
          if B.neq e.cover B.one then n1+1,n2 else n1, n2+1
        ) soup.elems (0,0)
    in
    Printf.printf "coverage: <1: %i, =1: %i\n" noncovered covered;
    if noncovered = 0 then Printf.printf "INDUCTIVE INVARIANT FOUND\n%!"
    else Printf.printf "FAILED\n%!";
    if !do_validate_final then validate soup

end
