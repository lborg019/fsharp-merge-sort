let rec split = function
    | [] -> ([], [])
    | [a] -> ([a], [])
    | a::b::cs -> let (M,N) = split cs in (a::M, b::N)

let rec merge = function
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                         else y :: merge (x::xs, ys)
    
let rec mergesort = function
    | [] -> []
    | [a] -> [a] //added line
    | L -> let (M, N) = split L in
                    merge (mergesort M, mergesort N);;

(*
  val mergesort : _arg1:'a list -> 'a list when 'a : comparison

  Mergesort returns a list that has the same type as it was 
  passed in.

  > mergesort [2;9;3;1;4;5;8;7;6];;
  val it : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
*)