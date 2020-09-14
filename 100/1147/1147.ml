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
    let n = scan "%d" id in
    if n = 0 then ()
    else
      let l = scan_lines n "%d" id  in
      (List.sort compare l
      |> List.mapi (fun i v -> if i = 0 || i = pred @@ List.length l then 0 else v)
      |> List.fold_left (+) 0 |> fun v -> Printf.printf "%d\n" (v/(n -2)));
      loop () in
  loop ()

