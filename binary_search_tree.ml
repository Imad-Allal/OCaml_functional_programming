type ('a, 'b) abr =
  Feuille of 'a * 'b
| Noeud of 'a * 'b * ('a, 'b) abr * ('a, 'b) abr


type empty = Null

let rec insert k v a = 
  match a with
  Feuille -> Noeud(k,v,empty,empty)
  |Noeud(k',v',fg,fd) -> if (k<=k') then
                          let fg' = insert k v fg
                          in Noeud(k',v',fg',fd)
                         else
                          let fd' = insert k v fd
                          in Noeud(k',v',fg,fd')
let a1 = insert 2 "deux" empty
let a2 = insert 9 "neuf" a1
let a3 = insert 4 "quatre" a2
let a4 = insert 7 "sept" a3
let a5 = insert 1 "un" a4
let a5 = insert 5 "cinq" a5
let a7 = insert 8 "huit" a6
let a8 = insert 11 "onze" a7

let rec printKeys a = 
  match a with
  Feuille -> ""
  |Noeud(k,v,fg,fd) -> "("^v^","^(printKeys fg)^","^(printKeys fd)^")"
  (** "^v^" = Forcer v qui est un int à s'afficher en string *)

let rec check a = 
  match a with 
  Feuille -> true
  |Noeud(_,_,Feuille,Feuille) -> true
  |Noeud(k,_,Noeud(k',_,fg',fd'),Feuille) -> (k' <= k) & (check Noeud(k', v', fg', fd'))
  |Noeud(k,_,Feuille,Noeud(k',v',fg',fd')) -> (k' > k) & (check Noeud(k', v', fg', fd'))
  |Noeud(k,_,Noeud(k',v',fg',fd'), Noeud(k'',v'',fg'',fd'')) & (check Noeud(k'',v'',fg'',fd'')) 
      ->(k' <= k) & (k''>k) & (check (Noeud(k',v',fg',fd')))
  ;;


type ('a, 'b) abr =
  Feuille of 'a * 'b
| Noeud of 'a * 'b * ('a, 'b) abr * ('a, 'b) abr


    type  expr = 
      Const of int
      | Var of String
      | Plus of expr * epxr
      | Minus of expr * epxr
      | Times of expr * epxr
      | Division of expr * epxr
    ;;
    let rec eval e = 
      match e with
      | Const(i) -> i
      | Plus(e1,e2) -> eval(e1) + eval(e2)
      | Minus(e1,e2) -> eval(e1) + eval(e2)
      | Times(e1,e2) -> eval(e1) * eval(e2)
      | Division(e1,e2) -> eval(e1) / eval(e2)
    let e = Times(Const 2,Plus(Const 3, Const 7 )) (**Return 20*)

    let rec check0 e = 
      match e with
      | Const(_) -> False
      | Plus(e1,e2) -> check0(e1) | check0(e2)
      | Minus(e1,e2) -> check0(e1) | check0(e2)
      | Times(e1,e2) -> check0(e1) | check0(e2)
      | Division(_,0) -> true
      | Division(e1,e2) -> check0(e1) | check0(e2)

    let rec propaConst e =
      match e with
      Const(_) -> e
      |Var(_) -> e
      |Plus(e1,e2) -> let e1' = propaConst e1 in
                       let e2' = propaConst e2 in
                        (match (e1',e2') with
                          (Const(i1), Const(i2)) -> Const(i1+i2)
                          | _ ->  Plus(e1',e2')
                        )
      |Times(e1,e2) -> let e1' = propaConst e1 in
                        let e2' = propaConst e2 in
                        (match (e1',e2') with
                          (Const(i1), Const(i2)) -> Const(i1*i2)
                          | _ ->  Times(e1',e2')
                        )
      |Minus(e1,e2) -> let e1' = propaConst e1 in
                        let e2' = propaConst e2 in
                        (match (e1',e2') with
                          (Const(i1), Const(i2)) -> Const(i1-i2)
                          | _ ->  Minus(e1',e2')
                        )
      |Divison(e1,e2) -> let e1' = propaConst e1 in
                    let e2' = propaConst e2 in
                    (match (e1',e2') with
                      (Const(i1), Const(i2)) -> Const(i1/i2)
                      | _ ->  Division(e1',e2')
                    );;
                    
    let e = Times(Var("x"), Plus(Const(3), Const(7)));;  


    exception PasTrouve;;
    
      let rec lookup k a = 
        match e with 
        Noeud(k',v,fg,fd) -> if (k=k') then v
                             else
                              if(k<k') then lookup k fg
                              else lookup k fd;;
        |Feuille -> raise PasTrouve;;
        
        let mya = Noeud(6,"six",Noeud(3,"trois", Feuille, Feuille),Noeud(9,"neuf",Feuille,Feuille));;

    let rec map a f = 
      match a with
      Feuille -> Feuille
      |Noeud(k,v,fg,fd) -> Noeud (k,f v,map fg f, map fd f);;
      let a7 = map a6 (fun s -> (s^"plus un"));;
    
      let rec fold a f e = 
        match a with
        Feuille -> e
        |Noeud (k,v,fg,fd) -> f v (f (fold fg f e) (fold fd f e));;
        fold a6 (fun x y -> (x^y)) "";;
      let fold2 a f e = 
        
        while (a != Feuille) do
          let Noeud(k,v,fg,fd) = a
          in 
                     
