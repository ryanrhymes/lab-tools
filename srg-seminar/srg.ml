open Lwt
open Cohttp
open Cohttp_lwt_unix


(** define basic fields in a talk *)
type email = string;;
type title = string;;
type abstract = string;;
type location = string;;
type datetime = string;;


(** retrieve the html page*)
let seminar_uri = "http://talks.cam.ac.uk/show/index/8316"
let seminar_page = Client.get (Uri.of_string seminar_uri) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body


let rex0 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<div class='vevent simpletalk click'>([\s\S]*)</div")
let rex1 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<h2.*><a.*href=\"(.+)\".*>([\s\S]*)</a></h2>")
let re00 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<h1 class='summary'.*>([\s\S]*)</h1>")
let re01 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"User\".*>([\s\S]*)</li>")
let re02 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"Clock\".*>([\s\S]*)</li>")
let re03 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "<.*abbr.*>"
let re04 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"House\".*><a.*>([\s\S]*)</a>.*</li>")
let re05 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p class='urgent'></p>([\s\S]*)<p>This talk is part of.*")
let re06 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p>([\s\S]*)</p>")
let re07 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "[\r\n]"
let re08 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "<.*>"
let re09 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "^\s+"
let re10 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "\s+$"
let re11 = Pcre.regexp ~flags:[`MULTILINE] "\s+"
let re12 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "&#8217;"


let get_all_tkuri s = 
  let ar0 = Pcre.extract_all ~rex:rex0 s in
  let ar1 = Array.map (fun x -> 
    let s = Array.get x 1 in 
    let y = Pcre.extract_all ~rex:rex1 s in
    Array.get (Array.get y 0) 1
  ) ar0 in
  return ar1


let get_talk_page talk_uri = 
  Client.get (Uri.of_string talk_uri) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body


let get_talk_details s = 
  let x = Pcre.extract_all ~rex:re00 s in
  let title = Array.get (Array.get x 0) 1 |> (Pcre.replace ~rex:re07) |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re09) in
  let x = Pcre.extract_all ~rex:re01 s in
  let speaker = Array.get (Array.get x 0) 1 |> (Pcre.replace ~rex:re07) |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re09) in
  let x = Pcre.extract_all ~rex:re02 s in
  let datetime = Array.get (Array.get x 0) 1 |> Pcre.replace ~rex:re03 |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re09) in
  let x = Pcre.extract_all ~rex:re04 s in
  let location = Array.get (Array.get x 0) 1 |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re09) in
  let x = Pcre.extract_all ~rex:re05 s in
  let abstract = Array.get (Array.get x 0) 1 |> (Pcre.extract_all ~rex:re06) |> (Array.map (fun x ->
    Array.get x 1 |> (Pcre.replace ~rex:re07) |> (Pcre.replace ~rex:re08) |> (Pcre.replace ~rex:re09) |> 
	(Pcre.replace ~rex:re10) |> (Pcre.replace ~rex:re11 ~templ:" ") |> (Pcre.replace ~rex:re12 ~templ:"\'")
  )) |> Array.to_list |> (String.concat "\n\n") in
  let r = "Date: " ^ datetime ^ "\n" ^
    "Location: " ^ location ^ "\n" ^
    "Speaker: " ^ speaker ^ "\n\n" ^
    "Title: " ^ title ^ "\n\n" ^
    "Abstact: " ^ abstract ^ "\n\n" ^
    "SRG Seminar: http://talks.cam.ac.uk/show/index/8316" in
  return r


let first_element x = return (Array.get x 0)
let get_comming_talk () = 
  seminar_page >>= get_all_tkuri >>= first_element >>=
  get_talk_page >>= get_talk_details |> Lwt_main.run |>
  print_endline


let get_talk_by_uri uri = 
  get_talk_page uri >>= get_talk_details |> Lwt_main.run |>
  print_endline


let _ = match Array.length Sys.argv with
  | 1 -> get_comming_talk ()
  | 2 -> get_talk_by_uri (Array.get Sys.argv 1)
  | _ -> print_endline "Too many arguments!"
