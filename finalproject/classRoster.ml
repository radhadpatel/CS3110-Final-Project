open Schedule

exception InvalidURL

(* Returns body of URL as string *)
let string_of_url url = 
  try
    let connection = Curl.init () and result = ref "" in
    Curl.set_writefunction connection
      (fun x -> result := !result ^ x; String.length x);
    Curl.set_url connection url;
    Curl.perform connection;
    Curl.global_cleanup ();
    !result
  with
    _ -> raise InvalidURL

(** [course_html name sem] is the html body from the class roster site for 
    course [name] during semester with id [sem].
    Rasies: [InvalidURL] if data cannot be obtained for any reason. *)
let course_html name sem =
  let c_num = Str.last_chars name 4 in
  let c_dep = Str.first_chars name ((String.length name) - 4) in
  let url = "https://classes.cornell.edu/browse/roster/" ^ 
            (string_of_semid sem) ^ "/class/" ^ c_dep ^ "/" ^ 
            c_num in
  string_of_url url

(** [parse_credits html] is the number of credits for the course whose class 
    roster webpage is stored as plain text in [html].
    Raises: [InvalidURL] if [html] doesn't contain this information. *)
let parse_credits html =
  let reg = Str.regexp_string {|<span class="credit-val">|} in
  try
    int_of_string (String.sub html ((Str.search_forward reg html 0) + 25) 1)
  with
    _ -> raise InvalidURL

let get_course_creds name sem =
  let n_upper = String.uppercase_ascii name in
  let reg = Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$" in
  if (Str.string_match reg n_upper 0) then
    parse_credits (course_html n_upper sem)
  else
    raise (UnknownCourse name)

(** [parse_title html] is [true] if title of the course whose class 
    roster webpage is stored as plain text in [html] begins with "FWS".
    Raises: [InvalidURL] if [html] doesn't contain this information. *)
let parse_title html =
  let reg1 = Str.regexp_string {|<div class="title-coursedescr">|} in
  let reg2 = Str.regexp_string {|>|} in
  try
    let idx = 
      (Str.search_forward reg1 html 0) + 31 |> Str.search_forward reg2 html
    in
    (String.sub html (idx + 1) 3) = "FWS"
  with
    _ -> raise InvalidURL

let get_FWS_status name sem =
  let n_upper = String.uppercase_ascii name in
  let reg = Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$" in
  if (Str.string_match reg n_upper 0) then
    parse_title (course_html n_upper sem)
  else
    raise (UnknownCourse name)

(** [parse_dist html] is the string containing the distribution categories
    as listed for a course whose class roster page is [html]. *)
let parse_dist html =
  let reg = Str.regexp_string {|<span class="catalog-distr">|} in
  let reg2 = Str.regexp_string {|</span>|} in
  try
    let srt = Str.search_forward reg2 html (Str.search_forward reg html 0) in
    let ed = Str.search_forward reg2 html (srt + 7) in
    String.sub html (srt + 7) (ed - 7 - srt) |> String.trim
  with
  | _ -> ""

let distribution_category name sem = 
  let n_upper = String.uppercase_ascii name in
  let reg = Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$" in
  if (Str.string_match reg n_upper 0) then
    parse_dist (course_html n_upper sem)
  else
    raise (UnknownCourse name)

let get_distribution_status name sem = 
  String.length (distribution_category name sem) <> 0

(** [parse_breadth html] is the string containing the breadth categories
    as listed for a course whose class roster page is [html]. *)
let parse_breadth html =
  let reg = Str.regexp_string {|<span class="catalog-breadth">|} in
  let reg2 = Str.regexp_string {|</span>|} in
  try
    let srt = Str.search_forward reg2 html (Str.search_forward reg html 0) in
    let ed = Str.search_forward reg2 html (srt + 7) in
    String.sub html (srt + 7) (ed - 7 - srt) |> String.trim
  with
  | _ -> ""

let breadth_category name sem = 
  let n_upper = String.uppercase_ascii name in
  let reg = Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$" in
  if (Str.string_match reg n_upper 0) then
    parse_breadth (course_html n_upper sem)
  else
    raise (UnknownCourse name)

let get_breadth_status name sem = 
  String.length (breadth_category name sem) <> 0