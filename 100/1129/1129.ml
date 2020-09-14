module ArrayL = ArrayLabels
module ListL = ListLabels

let dbg = Printf.printf "[debug]%s"

let max_num = 100_000_000_000

let id = fun x -> x
let tuple2 x y = (x,y)
let tuple3 x y z = (x,y,z)
let succ x = x + 1
let pred x = x - 1

let (++) n m =
  let rec aux i =
    if i = m then [m]
    else i :: aux (i+1) in
  if n > m then [] else aux n

let (++^) n m = n ++ (m-1)

let (--) n m = List.rev (m ++ n)

let scan fmt f = Scanf.sscanf (read_line ()) fmt f

let scan_list ~sep cnv =
  let line = read_line () in
  Str.split (Str.regexp_string sep) line
  |> List.map cnv

let scan_lines n fmt f =
  List.map (fun _ -> scan fmt f) (0++^n)

let scan_matrix n m e conv =
  let arr = Array.make_matrix n m e in
  Array.iteri (fun i line ->
      let s = Scanf.Scanning.from_string @@ read_line () in
      Array.iteri (fun j _ ->
          arr.(i).(j) <- Scanf.bscanf s " %s" conv;
        ) line) arr; arr

let rec take n  = function
  | [] -> []
  | x::ls ->
    if n = 0 then []
    else x::take (pred n) ls

let rec drop n = function
  | [] -> []
  | (_::ls) as l ->
    if n = 0 then l
    else drop (pred n) ls

let rec powerset = function
  | [] -> [[]]
  | hd::tl ->
    let pws =  powerset tl in
    pws @ ListL.map pws ~f:(fun pw -> hd::pw)

let permutations l =
  let rec interleave x = function
    | [] -> [[x]]
    | (hd::tl) as l ->
      (x::l) :: (interleave x tl |> ListL.map ~f:(fun l -> hd::l)) in
  let rec aux = function
    | [] -> [[]]
    | a::rest ->
      aux rest |> ListL.map ~f:(interleave a) |> List.concat in
  aux l

let between n x m = n <= x && x < m

let string_to_list s =
  List.map (String.get s) (0 ++^ String.length s)

let () =
  let rec loop () =
    let (n, r) = scan "%d %d" tuple2  in
    if n = 0 && r = 0 then ()
    else
      begin
        (scan_lines r "%d %d" tuple2
         |>  ListL.fold_left ~init:(n--1) ~f:(fun l (p, c) ->
             take c (drop (p-1) l) @ take (p-1) l @ drop (p-1+c) l)
         |> fun l -> Printf.printf "%d\n" @@ List.hd l);
        loop ()
      end
  in
  loop ()
