open Lwt
open Cohttp
open Cohttp_lwt_unix


(** define basic fields in a talk *)
type email = string;;
type title = string;;
type abstract = string;;
type bio = string;;


(** retrieve the html page*)
let seminar_uri = "http://talks.cam.ac.uk/show/index/8316"

let body = Client.get (Uri.of_string seminar_uri) >>= fun (resp, body) ->
  Cohttp_lwt_body.to_string body

(** parse html to get talks *)
let start_tag = "<div class='vevent simpletalk click'>"
let end_tag = "</div>"


let () = 
  let s = Lwt_main.run body in
  print_endline s
