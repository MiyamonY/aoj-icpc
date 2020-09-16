module ArrayL = ArrayLabels
module List = struct
  include ListLabels

  module Op = struct
    let (++) n m =
      let rec aux i =
        if i = m then [m]
        else i :: aux (i+1) in
      if n > m then [] else aux n

    let (++^) n m = n ++ (m-1)
    let (--) n m = List.rev (m ++ n)
    let (--^) n m = ((m+1) -- n)
  end

  open Op

  let init n ~f = ListLabels.map ~f (0++^n)

  let rec drop n ls =
    if n = 0 then ls
    else drop (n-1) ls

  let rec take n = function
    | [] -> []
    | x::ls ->
      if n = 0 then []
      else x :: take (pred n) ls

  let findi ~f ls =
    let rec aux i =  function
      | [] -> i
      | x::ls -> if f x then i else aux (i+1) ls
    in
    aux 0 ls

  let rec powerset = function
    | [] -> [[]]
    | hd::tl ->
      let pws =  powerset tl in
      pws @ ListLabels.map pws ~f:(fun pw -> hd::pw)

  let permutations l =
    let rec interleave x = function
      | [] -> [[x]]
      | (hd::tl) as l ->
        (x::l) :: (interleave x tl |> ListLabels.map ~f:(fun l -> hd::l)) in
    let rec aux = function
      | [] -> [[]]
      | a::rest ->
        aux rest |> ListLabels.map ~f:(interleave a) |> List.concat in
    aux l
end

open List.Op

let dbg = Printf.printf "[debug]%s"

let max_num = 100_000_000_000

let id = fun x -> x
let tuple2 x y = (x,y)
let tuple3 x y z = (x,y,z)
let succ x = x + 1
let pred x = x - 1

let scan fmt f = Scanf.sscanf (read_line ()) fmt f

let scan_list ~sep cnv =
  let line = read_line () in
  Str.split (Str.regexp_string sep) line
  |> List.map ~f:cnv

let scan_lines n fmt f =
  List.map ~f:(fun _ -> scan fmt f) (0++^n)

let scan_matrix n m e conv =
  let arr = Array.make_matrix n m e in
  Array.iteri (fun i line ->
      let s = Scanf.Scanning.from_string @@ read_line () in
      Array.iteri (fun j _ ->
          arr.(i).(j) <- Scanf.bscanf s " %s" conv;
        ) line) arr; arr

let between n x m = n <= x && x < m

let string_to_list s =
  List.map ~f:(String.get s) (0 ++^ String.length s)

let () =
  let rec loop () =
    let (n, p) = scan "%d %d" tuple2 in
    if n = 0 && p = 0 then ()
    else
      (let rec steps having p =
         if p = 0 && List.length @@ List.filter ~f:((<>) 0) having = 1 then List.findi ~f:((<>) 0) having
         else
           let rec step p aux = function
             | [] -> (p, aux)
             | x::ls -> if p = 0 then step x (0::aux) ls
               else step (pred p) (succ x::aux) ls
           in
           let (p, having) = step  p [] having in
           steps (List.rev having) p
       in
       let having = List.init n ~f:(fun _ -> 0) in
       steps having p |> Printf.printf "%d\n";
       loop ())
  in
  loop()
