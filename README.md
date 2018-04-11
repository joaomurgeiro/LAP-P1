# LAP-P1
LAP P1
(* RegExp module body *)
(*module List: sig .. end*)
(* 
Aluno 1: ????? mandatory to fill
Aluno 2: ????? mandatory to fill

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
01234567890123456789012345678901234567890123456789012345678901234567890123456789
   80 columns
*)


(* REGULAR EXPRESSIONS *)

type regExp =
      Str of string
    | Class of string
    | NClass of string
    | Any
    | Seq of regExp * regExp
    | Or of regExp * regExp
    | Not of regExp
    | Zero of regExp
    | ZeroOrOne of regExp
    | ZeroOrMore of regExp
    | OneOrMore of regExp
    | Repeat of int * int * regExp
;;


(* STRINGS *)

let cut s =
    (String.get s 0, String.sub s 1 ((String.length s)-1))
;;

let join x xs =
    (Char.escaped x)^xs
;;

(*passar de string para lista*)
let rec list_of_string s =
    if s = "" then []
    else
        let (x,xs) = cut s in
            x::list_of_string xs
;;

(*passar de lista para string*)
let rec string_of_list l =
    match l with
       [] -> ""
     | x::xs -> join x (string_of_list xs)
;;


(* matchAtStart *)

(*Examples*)
let z = matchAtStart "aabbcc" (Str "aa");;
val z : bool * string * string = (true, "aa", "bbcc")

let z = matchAtStart "aabbcc" (Str "ab");;
val z : bool * string * string = (false, "", "")

let z = matchAtStart "aaarest" (ZeroOrMore (Str "a"));;
val z : bool * string * string = (true, "aaa", "rest")




matchAtStartStr ['a';'a';'b';'b';'c';'c'] ['a';'a'];;
matchAtStartStr ['a';'a';'b';'b';'c';'c'] ['a';'b'];;

matchAtStartClass ['a';'a';'b';'b';'c';'c'] ['a';'b';'c'];;
matchAtStartClass ['a';'a';'b';'b';'c';'c'] ['a';'b'];;

matchAtStartNClass ['a';'a';'b';'b';'c';'c'] ['f';'d';'e'];;
matchAtStartNClass ['a';'a';'b';'b';'e';'c'] ['c';'d';'e'];;


let matchAtStart line re =
    let (b,m,r) = matchAtStartRE (list_of_string line) re in
        (b, string_of_list m, string_of_list r)
;;

let rec matchAtStartRE line re =
    match re with
      Zero p -> (true, [], line)
			| Any -> (true,  List.hd line, List.tl line)
			| Str s -> matchAtStartStr line (list_of_string s)
			| Class s -> matchStartClass line (list_of_string s)
			| NClass s -> matchStartNClass line (list_of_string s)
			| ZeroOrMore s -> matchAtStartZeroOrMore line (list_of_string s)
			| _ -> (false, [], [])
			(*| Class s ->*)
;;

let rec matchAtStartStr line s=
	match line,s with
		_, [] -> (true, [], line)
		| [],_ -> (false, [], [])
		|  x::xs , y::ys -> let (b,m,r) = matchAtStartStr xs ys in
												if x=y && b then (b, m@[y], r) else (false ,[] ,[])
;;

let rec matchAtStartClass line s=
	match line with
		[] -> (true, [],[])
		| x::xs -> if belongs x s then (true, [x] ,xs) else (false ,[] ,[])
;;

let rec matchAtStartNClass line s=
	match line with
		[] -> (true, [],[])
		| x::xs -> if not(belongs x s) then (true, [x] ,xs) else (false ,[] ,[])
;;

let rec belongs n l=
	match l with
		[] -> false
		| x::xs -> if n=x then true else belongs n xs
;;

let rec matchAtStartZeroOrMore line s=
	match line with
		[] -> []
		| x::xs

(*let rec matchAtStartClass line s=
	match line with
		[] -> (true, [],[])
		| x::xs -> let (b,m,r) = matchAtStartClass xs s in
							if belongs x s && b then (b, m@[x] ,r) else (false ,[] ,[])
;;*)



(* firstMatch *)

let rec firstMatchRE line re =
    (false,[],[],[])
;;

let firstMatch line re =
    let (b,p,m,r) = firstMatchRE (list_of_string line) re in
        (b, string_of_list p, string_of_list m, string_of_list r)
;;


(* allMatches *)

let rec allMatchesRE line re =
    []
;;

let allMatches line re =
    List.map
        (fun (p,m,r) -> (string_of_list p, string_of_list m, string_of_list r))
        (allMatchesRE (list_of_string line) re)
;;


(* replaceAllMatches *)

let rec replaceAllMatchesRE line rpl re =
    []
;;

let replaceAllMatches line rpl re =
    let lineStr = list_of_string line in
      let rplStr = list_of_string rpl in
        let res = replaceAllMatchesRE lineStr rplStr re in
          string_of_list res
;;


(* allMatchesFile *)

let allMatchesFile ni re =
    []
;;


(* allMatchesOverlap *)

let rec allMatchesOverlapRE line re =
    []
;;

let allMatchesOverlap line re =
    List.map
        (fun (p,m,r) -> (string_of_list p, string_of_list m, string_of_list r))
        (allMatchesOverlapRE (list_of_string line) re)
;;


(* matchAtStartGreedyRE *)

let matchAtStartGreedyRE line re =
    (false, [], [])
;;

let matchAtStartGreedy line re =
    let (b,m,r) = matchAtStartGreedyRE (list_of_string line) re in
        (b, string_of_list m, string_of_list r)
;;


