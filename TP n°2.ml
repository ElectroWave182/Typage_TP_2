(*
Exercice 1 :
------------
*)

print_endline ("Exercice 1 :\n------------") ;;



(* 1 - *)

type term =
    | VarTerm of string
    | AppTerm of string * term list
;;


(* 2 - *)

print_endline ("\n\n2 -\n") ;;

let a = VarTerm ("x") ;;

let b = AppTerm
(
    "f",
    [a; a]
)
;;

let c = AppTerm
(
    "f",
    [a; b]
)
;;

let d = AppTerm
(
    "g",
    [AppTerm
    (
        "f",
        [
            VarTerm ("z");
            AppTerm
            (
                "f",
                [a; VarTerm ("y")]
            )
        ]
    )]
)
;;

let e = AppTerm
(
    "g",
    [AppTerm
    (
        "f",
        [
            AppTerm
            (
                "f",
                [a; VarTerm ("y")]
            );
            AppTerm
            (
                "f",
                [a; VarTerm ("y")]
            )
        ]
    )]
)
;;


let rec termToString =
    let rec listToString =
    function
    
        (* Cas récursif *)
        | (tete :: cou :: queue) ->
            let appel = listToString (cou :: queue)
            in
                termToString (tete)
                ^ ", "
                ^ appel
                
        (* Cas de base : 1 seul élément *)
        | (dernier :: _) ->
            termToString (dernier)
            
        (* Cas élémentaire : [] *)
        | _ -> ""
    in
        function
            
            (* Cas récursif *)
            | (AppTerm (fonction, antecedents)) ->
                fonction
                ^ " ("
                ^ listToString (antecedents)
                ^ ")"
                
            (* Cas de base : Variable *)
            | (VarTerm (nom)) ->
                nom
;;

let afficheTerm =
function
    t ->
        print_endline (termToString (t))
;;


print_string ("a = ") ;;
afficheTerm (a) ;;

print_string ("b = ") ;;
afficheTerm (b) ;;

print_string ("c = ") ;;
afficheTerm (c) ;;

print_string ("d = ") ;;
afficheTerm (d) ;;


(*
6 - a = 2 * a <= a = 2
*)

(*
b = c <= a = f (a, a)
*)


(* 3 - *)

print_endline ("\n\n3 -\n") ;;

let appartient =
function
    cible ->
    
        let rec parcours =
        function
        
            (* Cas récursif *)
            | (tete :: queue)
            when cible <> tete
            ->
                parcours (queue)
            
            (* Cas de base : trouvée *)
            | (tete :: queue)
            -> true
            
            (* Cas de base : introuvable *)
            | _ -> false
        in
            parcours
;;
        
let rec union =
function
    
    (* Cas récursif *)
    | (tete :: queue, ens) ->
        if appartient (tete) (ens)
        then union (queue, ens)
        else union (queue, tete :: ens)
            
    (* Cas de base : [], _ *)
    | (_, ens) ->
        ens
;;

let rec fv =
    let rec listToFv =
    function
        
        (* Cas récursif *)
        | (tete :: queue) ->
            let appel = listToFv (queue)
            in
                union
                (
                    fv (tete),
                    appel
                )
                
        (* Cas de base : [] *)
        | _ -> []
    in
        function
            
            (* Cas récursif *)
            | (AppTerm (fonction, antecedents)) ->
                listToFv (antecedents)
                
            (* Cas de base : Variable *)
            | (VarTerm (nom)) ->
                [nom]
;;


let rec listToString =
function
    
    (* Cas récursif *)
    | (tete :: queue) ->
        let appel = listToString (queue)
        in
            tete
            ^ "; "
            ^ appel
            
    (* Cas de base : [] *)
    | _ -> ""
;;

let afficheListe =
function
    liste ->
        print_endline (listToString (liste))
;;


afficheListe (fv (c)) ;;
afficheListe (fv (d)) ;;



(*
Exercice 2 :
------------
*)

print_endline ("\n\n\nExercice 2 :\n------------") ;;


(* 1 - *)

let acces =
function
    cible ->
    
        let rec parcours =
        function
        
            (* Cas récursif *)
            | ((clef, _) :: queue)
            when cible <> clef
            ->
                parcours (queue)
            
            (* Cas de base : trouvée *)
            | ((_, valeur) :: _)
            -> valeur
            
            (* Cas de base : introuvable *)
            | _ -> VarTerm (cible)
        in
            parcours
;;


(* 2 - *)

print_endline ("\n\n2 -\n") ;;

let subst =
function

    env ->
        let rec parcours =
        function
            
            (* Cas récursif *)
            | (tete :: queue) ->
                remplacement (tete)
                :: parcours (queue)
                
            (* Cas de base : [] *)
            | _ -> []
            
        and remplacement =
        function
        
            (* Cas récursif *)
            | (AppTerm (fonction, liste)) ->
                AppTerm
                (
                    fonction,
                    parcours (liste)
                )
                
            (* Cas de base : variable *)
            | (VarTerm (nom)) ->
                acces (nom) (env)
        in
            remplacement
;;


let env =
[
    ("x", VarTerm ("v"));
    ("y", AppTerm ("f1", [VarTerm ("x")]));
    ("z", AppTerm ("f2", [VarTerm ("y")]))
] ;;

afficheTerm (subst (env) (d)) ;;


(* 3 - *)

print_endline ("\n\n3 -\n") ;;


let simplification =
function
    env ->
        
        let rec remplacement =
        function
            terme ->
                let appel = subst (env) (terme)
                in
                    if terme = appel
                    
                    (* Cas de base : rien à substituer *)
                    then terme
                    
                    (* Cas récursif *)
                    else remplacement (appel)
                    
        in
            remplacement
;;

afficheTerm (simplification (env) (d)) ;;



(*
Exercice 3 :
------------
*)

print_endline ("\n\n\nExercice 3 :\n------------\n\n") ;;


let unifie =
function
    env ->
        
        let rec parcours =
        function
            | (tete :: cou :: queue) ->
                if simplification (env) (tete)
                = simplification (env) (cou)
                
                (* Cas récursif *)
                then parcours (cou :: queue)
                
                (* Cas de base : pas unifiée *)
                else false
            
            (* Cas de base : moins de 2 éléments *)
            | _ -> true
            
        in
            parcours
;;


let afficheBool =
function
    bool ->
        print_endline (string_of_bool (bool))
;;


let envBis =
[
    ("z", AppTerm ("f", [VarTerm ("x"); VarTerm ("y")]))
] ;;

afficheBool (unifie (envBis) ([e ; d])) ;;
            