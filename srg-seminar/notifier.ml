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

let body = Client.get (Uri.of_string seminar_uri) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body

(** parse html to get talks *)
let start_tag = "<div class='vevent simpletalk click'>" and stop_tag = "</div>"
(** 
let talk_re = Str.regexp ("[.|\n]*" ^ start_tag ^ "\\(.+\\)" ^ stop_tag)
let get_all_talks s = 
  Str.string_match talk_re s 0;
  Str.matched_group 1 s
*)

let re0 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] (start_tag ^ "([\s\S]*)" ^ stop_tag)
let re1 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<h2.*><a.*href=\"(.+)\".*>([\s\S]*)</a></h2>")
let re2 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p class='details'><img.*>([\s\S]*)</p>")
let re3 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p class='location'><img.*><a.*>([\s\S]*)</a>.*</p>")
let re4 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p><img alt=\"Clock\".*>([\s\S]*)</p>")
let re5 = Pcre.regexp ~flags:[`UNGREEDY] "<.*abbr.*>"

let get_all_talks s = 
  let ar0 = Pcre.extract_all ~rex:re0 s in
  let ar1 = Array.map (fun x -> 
    let s = Array.get x 1 in 
    let y = Pcre.extract_all ~rex:re1 s in
    let tkrui = Array.get (Array.get y 0) 1 in
    let title = Array.get (Array.get y 0) 2 in
    let y = Pcre.extract_all ~rex:re2 s in
    let speaker = Array.get (Array.get y 0) 1 in
    let y = Pcre.extract_all ~rex:re3 s in
    let location = Array.get (Array.get y 0) 1 in
    let y = Pcre.extract_all ~rex:re4 s in
    let datetime = Array.get (Array.get y 0) 1 |> Pcre.replace ~rex:re5 in
    print_endline (title ^ " : " ^ speaker ^ " : " ^ location);
    print_endline (datetime);
    print_endline "*******"
  ) ar0 in
  "test"

let get_all_tkuri s = 
  let ar0 = Pcre.extract_all ~rex:re0 s in
  Array.map (fun x -> 
    let s = Array.get x 1 in 
    let y = Pcre.extract_all ~rex:re1 s in
    Array.get (Array.get y 0) 1
  ) ar0

let get_talk_page talk_uri = 
  Client.get (Uri.of_string talk_uri) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body

let re01 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"User\".*>([\s\S]*)</li>")
let re02 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"Clock\".*>([\s\S]*)</li>")
let re03 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] "<.*abbr.*>"
let re04 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<li><img alt=\"House\".*><a.*>([\s\S]*)</a>.*</li>")
let re05 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p class='urgent'></p>([\s\S]*)<p>This talk is part of.*")
let re06 = Pcre.regexp ~flags:[`UNGREEDY; `MULTILINE] ("<p>([\s\S]*)</p>")

let get_talk_details s = 
  let x = Pcre.extract_all ~rex:re01 s in
  let speaker = Array.get (Array.get x 0) 1 in
  let x = Pcre.extract_all ~rex:re02 s in
  let datetime = Array.get (Array.get x 0) 1 |> Pcre.replace ~rex:re03 in
  let x = Pcre.extract_all ~rex:re04 s in
  let location = Array.get (Array.get x 0) 1 in
  let x = Pcre.extract_all ~rex:re05 s in
  let abstract = Array.get (Array.get x 0) 1 |> (Pcre.extract_all ~rex:re06) in
  print_endline speaker;
  print_endline datetime;
  print_endline location;
  print_endline "abc";
  s

let _ = 
  let s = Lwt_main.run body in 
  let s = Array.get (get_all_tkuri s) 0 in
  let s = Lwt_main.run (get_talk_page s) in
  get_talk_details s
