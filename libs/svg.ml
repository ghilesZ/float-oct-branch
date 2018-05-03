(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Graphical output for images and animations, in SVG format.
 *)


open Syntax
open Abstract_sig

let total_time = ref 10.

    
module SVG(A:ABSTRACT) = struct

  module B = A.B
  

  (************************************************************************)
  (* TYPES *)
  (************************************************************************)


  (* box identifiers *)
  type id = int

  (* time, for animation *)
  type time = int

  (* geometry *)
  type elem = A.t
  type box2d = float * float * float * float

  (* obejct kind *)
  type kind =
    | Elem of float (* abstract element, with coverage *)
    | Part          (* partitioning *)
        

  type action =
    | Set of elem * kind
    | Rem
    

  (* file *)
  type svg = {
      enable: bool;                             (* if false, svg operations are ignored *)
      width: int; height: int;                  (* output size, in pixels *)
      x: var; y: var;                           (* coordinate variables *)
      tx:float; ty:float; sx:float; sy: float;  (* coordinate transform *)
      bound: elem; init: elem;
      elems: (id,(time*action) list) Hashtbl.t;
    }



  (************************************************************************)
  (* UTILITES *)
  (************************************************************************)


  let header = "<?xml version='1.0' standalone='no'?>\n<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n"
      
  let xmlns = "xmlns='http://www.w3.org/2000/svg'"


  (* rectangles *)
      
  (* elem to bounds *)
  let make_2d (x:var) (y:var) (b:elem) : box2d =
    let (x0,x1),(y0,y1) =  A.get_var_range b x, A.get_var_range b y in
    B.to_float_down x0, B.to_float_down y0,
    B.to_float_up   x1, B.to_float_up   y1 
      
  let transform (f:svg) ((x0,y0,x1,y1):box2d) : box2d =
    f.tx +. f.sx *. x0, f.ty +. f.sy *. y0,
    f.tx +. f.sx *. x1, f.ty +. f.sy *. y1
      
  let rect ch (f:svg) ?(fill="none") ?(stroke="black") ?(extra="") (b:elem) =
    let x0,y0,x1,y1 = transform f (make_2d f.x f.y b) in
    let x,y = min x0 x1, min y0 y1 in
    let w,h = max x0 x1 -. x, max y0 y1 -. y in  
    Printf.fprintf ch 
      "<rect fill='%s' stroke='%s' x='%f' y='%f' width='%f' height='%f' %s/>\n" 
      fill stroke x y w h extra
      
  let anim_rect ch (f:svg) ?(fill="none") ?(stroke="black") ?(extra="") (b:elem) t1 t2 =
    let x0,y0,x1,y1 = transform f (make_2d f.x f.y b) in
    let x,y = min x0 x1, min y0 y1 in
    let w,h = max x0 x1 -. x, max y0 y1 -. y in  
    Printf.fprintf ch 
      "<rect fill='%s' stroke='%s' x='%f' y='%f' width='%f' height='%f' visibility='hidden' %s>\n"
      fill stroke x y w h extra;
    Printf.fprintf ch "  <set attributeName='visibility' to='visible' begin='%fs'/>\n" t1;
    if t2 > 0. then
      Printf.fprintf ch "  <set attributeName='visibility' to='hidden' begin='%fs'/>\n" t2;
    Printf.fprintf ch "</rect>\n" 


      
  (* polygons *)

  let tr (f:svg) ((x,y):float*float) : float * float =
    f.tx +. f.sx *. x, f.ty +. f.sy *. y
      
  let poly ch (f:svg) ?(fill="none") ?(stroke="black") ?(extra="") (b:elem) =
    let l = A.to_polygon b f.x f.y in
    if l = [] then () else
    let l = List.map (tr f) l in
    Printf.fprintf ch "<polygon fill='%s' stroke='%s' points='" fill stroke;
    List.iter (fun (x,y) -> Printf.fprintf ch "%f,%f " x y) l;
    Printf.fprintf ch "' %s/>\n" extra

  let anim_poly ch (f:svg) ?(fill="none") ?(stroke="black") ?(extra="") (b:elem) t1 t2 =
    let l = A.to_polygon b f.x f.y in
    if l = [] then () else
    let l = List.map (tr f) l in
    Printf.fprintf ch "<polygon fill='%s' stroke='%s' points='" fill stroke;
    List.iter (fun (x,y) -> Printf.fprintf ch "%f,%f " x y) l;
    Printf.fprintf ch "' visibility='hidden' %s>\n" extra;
    Printf.fprintf ch "  <set attributeName='visibility' to='visible' begin='%fs'/>\n" t1;
    if t2 > 0. then
      Printf.fprintf ch "  <set attributeName='visibility' to='hidden' begin='%fs'/>\n" t2;
    Printf.fprintf ch "</polygon>\n" 


      
      

  (************************************************************************)
  (* DATA MANAGEMENT *)
  (************************************************************************)


  let create 
      (width:int) (height:int) (x:var) (y:var) 
      (init:elem)  (goal:elem)
      : svg
      =
    let enable = x<>"" && y<>"" in
    let x0,y1,x1,y0 = if enable then make_2d x y goal else 0.,0.,1.,1. in
    let w,h = float_of_int width, float_of_int height in
    { enable; x; y;
      width; height;
      tx = w *. x0 /. (x0 -. x1);
      ty = h *. y0 /. (y0 -. y1);
      sx = w /. (x1 -. x0);
      sy = h /. (y1 -. y0);
      bound = goal; init;
      elems = Hashtbl.create 16;
    }
      
      
  let add_elem (f:svg) (id:id) (t:time) (elem:elem) (kind:kind) =
    if f.enable then 
      Hashtbl.replace f.elems id [t,Set (elem,kind)]
        
  let change_elem (f:svg) (id:id) (t:time) (elem:elem) (kind:kind) =
    if f.enable then
      if Hashtbl.mem f.elems id then
        let p = Hashtbl.find f.elems id in
        match p with
        | (_, Set (elem',kind'))::_ when A.equal elem elem' && kind=kind' 
          -> ()
        | _ -> Hashtbl.replace f.elems id ((t,Set (elem,kind))::p)
      else add_elem f id t elem kind
          
  let delete_elem (f:svg) (id:id) (t:time) =
    if f.enable && Hashtbl.mem f.elems id then
      let p = Hashtbl.find f.elems id in
      Hashtbl.replace f.elems id ((t,Rem)::p)
        
        

  (************************************************************************)
  (* OUTPUT *)
  (************************************************************************)


  let kind_color c = match c with
  | Elem 1. -> Some "cornflowerblue"
  | Elem 0. -> Some "red"
  | Elem _ -> Some "lightsalmon"
  | Part -> None
      


  (* single image, using the most recent elem state *)
  let save_image (filename:string) (f:svg) =
    if not f.enable then () else
    
    (* header *)
    let ch = open_out filename in
    Printf.fprintf ch "%s<svg width='%ipx' height='%ipx' %s>\n" header f.width f.height xmlns;
    
    (* background *)
    Printf.fprintf ch "<rect fill='white' stroke='none' x='%i' y='%i' width='%i' height='%i'/>\n" 0 0 f.width f.height;
    
    (* elems *)
    Hashtbl.iter
      (fun id l -> match snd (List.hd l) with
      | Rem -> ()
      | Set (b,c) -> (*rect*) poly ch f ~stroke:"black" ?fill:(kind_color c) b
      ) f.elems;
    
    (* init *)
    (*rect*) poly ch f ~stroke:"darkblue" ~extra:"stroke-dasharray='1'" f.init;
    
    (* footer *)
    Printf.fprintf ch "</svg>\n";
    close_out ch
      

      

  (* animation. from the begining of time up to most recent time *)
  (* TODO: skip some animation steps to reduce the animation size *)
  let save_animation (filename:string) (f:svg) =
    if not f.enable then () else
    
    (* header *)
    let ch = open_out filename in
    Printf.fprintf ch "%s<svg width='%ipx' height='%ipx' %s>\n" header f.width f.height xmlns;
    
    (* background *)
    Printf.fprintf ch "<rect fill='white' stroke='none' x='%i' y='%i' width='%i' height='%i'/>\n" 0 0 f.width f.height;
    
    let max_time = Hashtbl.fold (fun _ l a -> max a (fst (List.hd l))) f.elems 0 in
    let time t = float_of_int t *. !total_time /. float_of_int max_time in
    
    (* elems *)
    Hashtbl.iter
      (fun id l ->
        let rec bb = function
          | (t1, Set (b,c))::(((t2,_)::_) as r) ->
              (*anim_rect*) anim_poly ch f ~stroke:"black" ?fill:(kind_color c) b 
                (time t1) (time t2);
              bb r
          | [t1, Set (b,c)] ->
              (*anim_rect*) anim_poly ch f ~stroke:"black" ?fill:(kind_color c) b 
                (time t1) (-.1.)
          | _ -> ()
        in
        bb (List.rev l)
      ) f.elems;
    
    (* init *)
    (*rect*) poly ch f ~stroke:"darkblue" ~extra:"stroke-dasharray='1'" f.init;
    
    (* footer *)
    Printf.fprintf ch "</svg>\n";
    close_out ch

end
