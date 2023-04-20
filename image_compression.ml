let _ = Graphics.open_graph " 512x512"

type couleur = Blanc |Noir

type arbre =
  |Feuille of couleur
  |Noeud   of arbre * arbre * arbre * arbre

type image = couleur array array


let rec compte_feuille x = 
  match x with
  Feuille coul -> 1
  |Noeud(a,b,c,d) -> compte_feuille a + compte_feuille b + compte_feuille c + compte_feuille d 

let a =
  Noeud (Noeud (Feuille Blanc,
                Feuille Blanc,
                Feuille Noir,
                Feuille Blanc),
          Feuille Noir,
          Feuille Noir,
          Feuille Noir)

let () = assert (compte_feuilles a = 7)


let dessine_arbre k a =
  let rec dessin k a b c d =
    match a with
    Feuille Blanc -> ()
    |Feuille Noir ->  Graphics.fill_rect k k b c d
    |Noeud (w,x,y,z) ->
        dessin b (c+d) d w ;
        dessin (b+d) (c+d) d x ;
        dessin b c d y ;
        dessin (b+d) c d z
  in dessin k a 0 0 (k/2)

(**************)
(**** TEST ****)

let rec q2 a =
  Graphics.clear_graph ();
  dessine_arbre 512 a;
  let rec do_rec () =
    let c = Graphics.read_key () in
    if c = 'q' then ()
    else do_rec ()
  in
  do_rec ()

let () = q2 a

(** FIN TEST **)
(**************)

  
let image_vers_arbre k image = 
  let rec image_abr k image i j k2 =
    if k <= 1 then Feuille image.(i).(j)
    else
      let a = image_abr k image i (j+k2) k2
      and b = image_abr k image (i+k2) (j+k2) k2
      and c = image_abr k image i j k2
      and d = image_abr k image (i+k2) j k2
      in match w,x,y,z with
      Feuille w, Feuille x, Feuille y, Feuille z when w = x && w = y && w = z -> a
      |_,_,_,_ ->  Noeud (a,b,c,d)
  in image_abr k image 0 0 (k/2)

(**************)
(**** TEST ****)

let img =
  [|
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir; Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ;Blanc ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    [| Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;
        Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ;Noir ; |] ;
    |]
  [@@ocamlformat "disable"]
  
  let () = assert (image_vers_arbre (Array.length img) img = a)

(** FIN TEST **)
(**************)


let rec inverse a =
  match a with
  Feuille Blanc -> Feuille Noir
  |Feuille Noir -> Feuille Blanc
  |Noeud (w,x,y,z) -> Noeud (inverse w, inverse x, inverse y, inverse z)

let rec rotate a =
  match a with
  Feuille _ -> a
  |Noeud (w,x,y,z) -> Noeud (rotate x,rotate z, rotate w, rotate y)

let rec antirotate a =
  match a with
  |Feuille _ -> a
  |Noeud (w,x,y,z) -> Noeud (antirotate y,antirotate w, antirotate z, antirotate x)

(**************)
(**** TEST ****)

let rec q4 a =
  Graphics.clear_graph ();
  dessine_arbre 512 a;
  let rec do_rec () =
    let c = Graphics.read_key () in
    if c = 'n' then q4 (rotate a)
    else if c = 'p' then q4 (antirotate a)
    else if c = 'i' then q4 (inverse a)
    else if c = 'q' then ()
    else do_rec () in
  do_rec ()

let () = q4 a

(** FIN TEST **)
(**************)


let rec fractale n =
  if n <= 0 then Feuille Noir
  else
     let a = fractale (n-1)
     in let b = Noeud (a,a,a,Feuille Blanc)
     in let c = rotate b
     in let d = rotate c
     in let e = rotate d
     in Noeud (b,c,d,e)

(**************)
(**** TEST ****)

let rec q5 i =
  dessine_arbre 512 (fractale i);
  let rec do_rec () =
    let c = Graphics.read_key () in
    if c = 'n' && i < 5 then begin
      Graphics.clear_graph ();
      q5 (i+1)
    end else if c = 'p' && i > 0 then begin
      Graphics.clear_graph ();
      q5 (i-1)
    end else if c = 'q' then
      ()
    else
      do_rec () in
  do_rec ()

let () = q5 0

(** FIN TEST **)
(**************)

type bit = Z | U

let  arbre_vers_liste a =
  let rec arbre a k = 
    match a with
    Feuille Blanc -> Z::Z::k
    |Feuille Noir  -> Z::U::k
    |Noeud (w,x,y,z) -> U::arbre w (arbre x (arbre y (arbre z k)))
  in arbre a []

(* Reciproque *)

let rec liste_vers_arbre lst =
  match lst with
  Z::Z::f -> Feuille Blanc,f
  |Z::U::f -> Feuille Noir,f
  |U::f -> let w,f = liste_vers_arbre f
        in let x,f = liste_vers_arbre f
        in let y,f = liste_vers_arbre f
        in let z,f = liste_vers_arbre f
        in Noeud (w,x,y,z),f
  |_ -> 0

(**************)
(**** TEST ****)

let a = fractale 4
let () = assert (a = liste_vers_arbre (arbre_vers_liste a))

(** FIN TEST **)
(**************)


let binaire bit =
  match bit with
  Z -> 0
  |U -> 1

let unbit lst =
  match lst with
  [] -> Z,[]
  |e::f -> (e,f)
;;

let rec nbit n lst =
  if n <= 0 then (0,lst)
  else
    let e,f = unbit lst
    in let g,f = nbit (n-1) f
    in (2*g) + ((binaire b),f)

let rec afficher lst l =
  match l with
  [] -> ()
  |_ -> let (rst,b) = nbit lst 8 in
    output_char (Char.chr rst) l;
    output_list l b

let ecrire_arbre a nom =
  let o = open_out_bin nom in
  afficher (arbre_vers_liste a) o;
  close_out o

(**************)
(**** TEST ****)

let q7 a =
  ecrire_arbre "f4.quad" a ;
  let aa = lire_arbre "f4.quad" in
  assert (a = aa)

let () = q7 (fractale 4)

(** FIN TEST **)
(**************)