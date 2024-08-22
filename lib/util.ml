open Printf
include Fun

let sp = sprintf
let p = printf
let ep = eprintf
let sl map sep lst = lst |> List.map map |> String.concat sep
let ( >>= ) = Option.bind
let ( $> ) op f = Option.map f op
let ( >>> ) f g x = x |> f |> g

module Set = struct
  type 'a t = 'a list

  let rec union xs = function
    | [] -> xs
    | hd :: tl ->
        if List.find_opt (fun x -> x = hd) xs |> Option.is_some then union xs tl
        else union (hd :: xs) tl

  let from_list lst =
    let rec helper acc = function
      | [] -> acc
      | hd :: tl -> helper (union acc [ hd ]) tl
    in
    helper [] lst
end

module List = struct
  include List

  let group (n : int) (lst : 'a list) =
    let rec aux acc m lst =
      match (lst, acc) with
      | [], [] -> []
      | [], x -> [ rev x ]
      | hd :: tl, x ->
          if m = 0 then rev x :: aux [] n (hd :: tl)
          else aux (hd :: x) (m - 1) tl
    in
    aux [] n lst

  let inspect f lst =
    List.iter f lst;
    lst
end

let sl_grouped n map sep sep_line lst =
  lst |> List.group n |> List.map (sl map sep) |> sl id sep_line

module Option = struct
  include Option

  let unwrap_or default = fold ~none:default ~some:id

  (** fish operator *)
  let ( ><> ) opt or_else =
    match opt with Some x -> Some x | None -> or_else ()

  let unwrap ?(exn = Failure "Unwrapped a None value") = function
    | None -> raise exn
    | Some x -> x
end
