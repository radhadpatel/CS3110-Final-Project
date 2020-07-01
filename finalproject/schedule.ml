type grade = Sat | Unsat | Withdrawn | Incomplete | None | Transfer 
           | Letter of string

type sem_id = Spring of int | Fall of int | None

type category = PE | FWS | ENGRI | ENGRD | Required | Core | FourThousandPlus | 
                Technical | Specialization| Liberal | AdvisorApproved | 
                MajorApproved | Practicum | Extra | ForeignLanguage | 
                PBS | GB | HB

type course = {
  name: string;
  mutable credits: int;
  mutable grade: grade;
  mutable category: category;
}

type semester = {
  mutable id: sem_id;
  mutable courses: course list;
  mutable tot_credits: int;
  mutable sem_gpa: float;
}

type settings = {
  mutable autosave: bool;
  mutable html_background: string;
  mutable html_squares: string;
}

type validation = {
  sch: string;
  needed: string list;
  needed_cat: (string * int) list;
  needed_subs : string list list
}

type schedule = {
  mutable desc: string;
  mutable semesters: semester list;
  mutable cumul_gpa: float;
  mutable sch_credits : int;
  mutable is_saved : bool;
  mutable settings : settings;
  mutable school : string;
  mutable valid : validation option
}

exception UnknownCourse of string
exception UnknownSemester of string
exception UnknownGrade of string
exception UnknownCategoryENG of string
exception UnknownCategoryCAS of string
exception UnknownSetting of string
exception UnknownSchool of string
exception DuplicateCourse of string
exception DuplicateSemester of string
exception InvalidCredits of string
exception InvalidSwap
exception InvalidMove
exception InvalidAttribute of string

let string_of_list str_lst = 
  let str = List.fold_left (fun acc str -> acc ^ str ^ ", ") "[ " str_lst in
  Str.replace_first (Str.regexp ", $") " ]" str

(** [grade_map gr] is the grade point value for [gr] if [gr] is a letter grade.
    Is -1.0 otherwise. *)
let grade_map gr = 
  match gr with
  | Letter "A+" -> 4.3
  | Letter "A" -> 4.0
  | Letter "A-" -> 3.7
  | Letter "B+" -> 3.3
  | Letter "B" -> 3.0
  | Letter "B-" -> 2.7
  | Letter "C+" -> 2.3
  | Letter "C" -> 2.0
  | Letter "C-" -> 1.7
  | Letter "D+" -> 1.3
  | Letter "D" -> 1.0
  | Letter "D-" -> 0.7
  | Letter "F" -> 0.0
  | _ -> -1.0

let gradify str =
  let str_upper = String.uppercase_ascii str in
  if Str.string_match (Str.regexp "^[A-D][\\+-]?$\\|^F$") str_upper 0 then
    Letter str_upper
  else
    match str_upper with
    | "INCOMPLETE" | "INC" -> Incomplete
    | "W" | "WITHDRAWN" -> Withdrawn
    | "SAT" | "S" -> Sat
    | "UNSAT" | "U" -> Unsat
    | "NONE" -> None
    | "TRANSFER" -> Transfer
    | _ -> raise (UnknownGrade str)

let check_school school =
  match school with
  | "CAS" | "ENG" -> true
  | _ -> raise (UnknownSchool school)

let categorify str =
  match String.uppercase_ascii str with
  | "PE" -> PE
  | "FWS" -> FWS
  | "ENGRI" -> ENGRI
  | "ENGRD" -> ENGRD
  | "REQ" | "REQUIRED" -> Required
  | "CORE" -> Core
  | "4000+" -> FourThousandPlus
  | "TECH" | "TECHNICAL" | "TECHNICAL ELECTIVE" -> Technical
  | "SPCL" | "EXT" | "EXTERNAL SPECIALIZATION" -> Specialization
  | "LIBERAL STUDIES" | "LIBERAL" | "MQR" | "CA" 
  | "HA" | "KCM" | "LA" | "SBA" -> Liberal
  | "APRV" | "ADVISOR" | "ADVISOR-APPROVED ELECTIVE" -> AdvisorApproved
  | "MAJ" | "MAJOR" | "MAJOR-APPROVED ELECTIVE" -> MajorApproved
  | "PROJECT" | "PROJ" | "PRACTICUM" | "PRACT" -> Practicum
  | "EXTRA" | "EXTRA COURSE" -> Extra
  | "LANG" | "LANGUAGE" | "FOREIGN" 
  | "FOREIGN LANGUAGE" -> ForeignLanguage
  | "PBS" | "PBSS" -> PBS
  | "GB" -> GB
  | "HB" -> HB
  | _ -> raise (UnknownCategoryENG str)

let gpa courses =
  let rec fold_credits courses acc =
    match courses with
    | [] -> acc
    | { credits = c; grade = g } :: t -> 
      if (grade_map g >= 0.) then fold_credits t (acc + c)
      else fold_credits t acc
  in
  let rec fold_gps courses acc =
    match courses with
    | [] -> acc
    | { credits = c; grade = g } :: t -> 
      if (grade_map g >= 0.) then 
        fold_gps t (acc +. ((float_of_int c) *. grade_map g))
      else fold_gps t acc
  in
  (fold_gps courses 0.) /. (float_of_int (fold_credits courses 0))

let gpa_to_string gpa_float = 
  let gpa = string_of_float gpa_float in
  match String.length gpa with
  | 0 | 1 -> failwith "Impossible Case"
  | 2 -> gpa ^ "00"
  | 3 -> 
    if gpa = "nan" then "0.00"
    else gpa ^ "0"
  | 4 -> 
    if gpa = "-nan" then "0.00"
    else gpa
  | _ -> Str.first_chars gpa 4

let get_gpa sch = 
  sch.cumul_gpa

let get_credits sch = 
  sch.sch_credits

let calc_credits courses =
  let rec fold courses acc =
    match courses with
    | [] -> acc
    | { credits = c } :: t -> fold t (acc + c)
  in fold courses 0

let to_list sch =
  List.fold_left (fun acc sem -> acc @ sem.courses) [] sch.semesters

let string_of_semid semid =
  match semid with
  | Spring yr -> "SP" ^ (string_of_int yr)
  | Fall yr -> "FA" ^ (string_of_int yr)
  | None -> "None"

let string_of_grade gr =
  match gr with
  | Sat -> "Satisfactory"
  | Unsat -> "Unsatisfactory"
  | Withdrawn -> "Withdrawn"
  | Incomplete -> "Incomplete"
  | None -> "None"
  | Transfer -> "Transfer"
  | Letter l -> l

let string_of_category cat =
  match cat with
  | PE -> "PE"
  | FWS -> "FWS"
  | ENGRI -> "ENGRI"
  | ENGRD -> "ENGRD"
  | Required -> "Required"
  | Core -> "Core"
  | FourThousandPlus -> "4000+"
  | Technical -> "Technical Elective"
  | Specialization -> "External Specialization"
  | Liberal -> "Liberal Studies"
  | AdvisorApproved -> "Advisor-Approved Elective"
  | MajorApproved -> "Major-Approved Elective"
  | Practicum -> "Practicum"
  | ForeignLanguage -> "Foreign Language"
  | GB -> "GB"
  | HB -> "HB"
  | PBS -> "PBS"
  | Extra -> "Extra Course"

(** [sem_compare s1 s2] is a negative number if [s1] comes before [s2], 
    0 if theyre the same semester, and a positive number if [s1] comes after
    [s2]. *)
let sem_compare s1 s2 =
  match s1.id,s2.id with
  | Fall y1 , Fall y2
  | Spring y1 , Spring y2 -> Stdlib.compare y1 y2
  | Fall y1 , Spring y2 -> if y1 = y2 then 1 else Stdlib.compare y1 y2
  | Spring y1 , Fall y2 -> if y1 = y2 then -1 else Stdlib.compare y1 y2
  | _ -> failwith "Impossible case."

let create_course name cred gr cat = 
  if cred < 0 then 
    raise (InvalidCredits "Credits have to be greater than or equal to zero.")
  else if not (Str.string_match
                 (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") name 0) then
    raise (UnknownCourse ("Invalid Course name - " ^ name))
  else
    {
      name = name;
      credits = cred;
      grade = gr;
      category = cat;
    }

let rec get_course name courses = 
  match courses with 
  | [] -> raise (UnknownCourse name)
  | h :: t -> if h.name = name then h else get_course name t

let get_course_name course =
  course.name

let get_course_credits course = 
  course.credits

let get_course_cat course sch =
  string_of_category course.category

let get_sem sch semid = 
  let rec get_sem_loop sems semid' = 
    match sems with 
    | [] -> raise (UnknownSemester (string_of_semid semid'))
    | h :: t -> if h.id = semid then h else get_sem_loop t semid'
  in
  get_sem_loop sch.semesters semid

let get_sems sch = 
  sch.semesters

let get_sem_courses sem =
  sem.courses 

let add_course sch c semid = 
  try
    let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
    if List.mem c.name (List.map (fun c -> c.name) (to_list sch)) then
      raise (DuplicateCourse (c.name ^ " already in schedule."))
    else begin
      sem.courses <- (c :: sem.courses);
      sem.sem_gpa <- gpa sem.courses;
      sem.tot_credits <- sem.tot_credits + c.credits;
      sch.sch_credits <- sch.sch_credits + c.credits;
      sch.cumul_gpa <- gpa (to_list sch); 
      sch.is_saved <- false;
      sch
    end
  with
    Not_found -> raise (UnknownSemester (string_of_semid semid))

(** [get_sem_from_course sems course] is the semester that [course] belongs to
    given list of semesters [sems]. 
    Raises: [UnkownCourse] if no sem found. *)
let rec get_sem_from_course sems course = 
  match sems with 
  | [] -> raise (UnknownCourse course.name)
  | h :: t ->
    if List.mem course h.courses then h
    else get_sem_from_course t course

let edit_course sch cname attr new_val =
  try
    let course = List.find (fun course -> course.name = cname) (to_list sch) in
    let sem = get_sem_from_course sch.semesters course in
    let old_creds = course.credits in
    match attr with
    | "credits" ->
      course.credits <- int_of_string new_val;
      let diff = int_of_string new_val - old_creds in 
      sem.tot_credits <- sem.tot_credits + diff;
      sch.sch_credits <- sch.sch_credits + diff;
      sem.sem_gpa <- gpa sem.courses;
      sch.cumul_gpa <- gpa (to_list sch); sch.is_saved <- false; sch
    | "grade" -> 
      course.grade <- gradify new_val;
      sem.sem_gpa <- gpa sem.courses;
      sch.cumul_gpa <- gpa (to_list sch); sch.is_saved <- false; sch
    | "category" -> 
      course.category <- categorify (String.uppercase_ascii new_val); 
      sch.is_saved <- false; sch
    | _ -> raise (InvalidAttribute attr)
  with
    Not_found -> raise (UnknownCourse cname)

let remove_course sch cname =
  try
    let course = get_course cname (to_list sch) in
    let sem = get_sem_from_course sch.semesters course in
    sem.courses <- (List.filter (fun crs -> crs.name <> cname) sem.courses);
    sem.tot_credits <- sem.tot_credits - course.credits;
    sch.sch_credits <- sch.sch_credits - course.credits;
    sem.sem_gpa <- gpa sem.courses;
    sch.cumul_gpa <- gpa (to_list sch); 
    sch.is_saved <- false;
    sch
  with 
    Not_found -> raise (UnknownCourse cname)

let swap_courses c1_name c2_name sch =
  let c1 = get_course c1_name (to_list sch) in
  let sem1 = get_sem_from_course sch.semesters c1 in
  let c2 = get_course c2_name (to_list sch) in
  let sem2 = get_sem_from_course sch.semesters c2 in
  if sem1.id = sem2.id then 
    raise InvalidSwap
  else
    let sch' = remove_course (remove_course sch c1.name) c2.name in
    add_course (add_course sch' c1 sem2.id) c2 sem1.id

let move_course c_name semid sch =
  let course = get_course c_name (to_list sch) in
  let old_sem = get_sem_from_course sch.semesters course in
  let sem = List.find (fun sm -> sm.id = semid) sch.semesters in
  if old_sem.id = sem.id then
    raise InvalidMove
  else
    let sch' = remove_course sch c_name in
    add_course sch' course sem.id

let sem_ids sch =
  List.rev_map (fun sem -> sem.id) sch.semesters

let sem_ids_to_string sch =
  List.rev_map (fun sem -> string_of_semid sem.id) sch.semesters

let create_sem semid =
  {
    id = semid;
    courses = [];
    tot_credits = 0;
    sem_gpa = 0.
  }

let add_sem sch sem =
  if (List.mem sem.id (sem_ids sch)) then
    raise (DuplicateSemester (string_of_semid sem.id))
  else
    sch.semesters <- List.sort sem_compare (sem :: sch.semesters); 
  sch.is_saved <- false; sch

let remove_sem sch semid = 
  if (not (List.mem semid (sem_ids sch))) then
    raise (UnknownSemester (string_of_semid semid))
  else begin
    sch.semesters <- 
      (List.filter (fun sem -> sem.id <> semid) sch.semesters); 
    sch.cumul_gpa <- gpa (to_list sch);
    sch.sch_credits <- calc_credits (to_list sch);
    sch.is_saved <- false;
    sch end

let new_schedule name school =
  let default_settings = {
    autosave = false;
    html_background = "rgb(255, 224, 198)";
    html_squares = "rgb(206, 225, 231)"
  } 
  in
  {
    desc = name;
    semesters = [];
    cumul_gpa = 0.;
    sch_credits = 0;
    is_saved = true;
    settings = default_settings;
    school = school;
    valid = None
  }

let get_save_status sch = 
  sch.is_saved

let set_save_status sch b = 
  sch.is_saved <- b

let get_name sch =
  sch.desc

let get_school sch =
  sch.school

let set_school sch school =
  sch.school <- school

let edit_name sch nm =
  sch.desc <- nm;
  sch.is_saved <- false;
  sch

let edit_settings sch attr new_val =
  match attr with
  | "autosave" -> 
    sch.settings.autosave <- bool_of_string new_val; sch
  | "html_bg_color" -> 
    sch.settings.html_background <- new_val; sch
  | "html_tile_color" ->
    sch.settings.html_squares <- new_val; sch
  | _ -> raise (UnknownSetting attr)

let set_valid sch v = 
  sch.valid <- v

let print_course sch course =
  print_newline ();
  ANSITerminal.(print_string [Bold] course.name); print_newline ();
  print_endline ( "Credits: " ^ (string_of_int course.credits) );
  print_endline ( "Grade: " ^ (string_of_grade course.grade) );
  print_endline ( "Category: " ^ (string_of_category course.category));
  let semid = (get_sem_from_course sch.semesters course).id in
  print_endline ("Semester: " ^ string_of_semid semid)

let print_sem sem =
  print_string ((string_of_semid sem.id) ^ ": ");
  print_endline (string_of_list (List.map (fun x -> x.name) sem.courses));
  print_string ("Semester GPA: " ^ (gpa_to_string sem.sem_gpa));
  print_endline (" | Semester Credits: " ^ string_of_int sem.tot_credits);
  print_newline ()

let print_schedule sch =
  if sch.semesters = [] then 
    ANSITerminal.(
      print_string [red] 
        "No semesters in current schedule. Try running 'add <semester>'\n")
  else begin
    List.fold_left (fun () sem -> print_sem sem) () sch.semesters;
    print_endline ("Cumulative GPA: " ^ (gpa_to_string sch.cumul_gpa));
    print_endline ("Total Credits: " ^ (string_of_int sch.sch_credits))
  end

module HTML = struct

  (** [template] is the string of raw HTML text from temp.html template file. *)
  let template =
    let rec input_file acc chan = 
      try
        input_file (acc ^ ((input_line chan) ^ "\n")) chan
      with
        End_of_file -> acc
    in
    input_file "" (open_in "temp.html")

  (** [html_of_course c] is a string representation of [c] in raw, 
      valid HTML form. *) 
  let html_of_course c =
    "\t\t\t\t<td>\n" ^ 
    "\t\t\t\t\t<h4><strong>" ^ c.name ^ "</strong></h4>\n" ^ 
    "\t\t\t\t\t<p>Credits: " ^ (string_of_int c.credits) ^ "</p>\n" ^
    "\t\t\t\t\t<p>Grade: " ^ (string_of_grade c.grade) ^ "</p>\n" ^ 
    "\t\t\t\t\t<p>Category: " ^ (string_of_category c.category) ^ "</p>\n" ^ 
    "\t\t\t\t</td>\n"

  (** [html_of_sem sem] is a string representation of [sem] in raw, 
      valid HTML form. *)  
  let html_of_sem sem =
    match sem.courses with
    | [] -> "\t\t\t<tr><td class=\"noborder\"><h3>" ^ (string_of_semid sem.id) ^ 
            "</h3></td></tr>\n"
    | _ -> begin
        "\t\t\t<tr><td class=\"noborder\"><h3>" ^ (string_of_semid sem.id) ^ 
        "</h3>\n" ^
        "\t\t\t<p>Semester GPA: <strong>" ^ (gpa_to_string sem.sem_gpa) ^ 
        "</strong></p></td>\n" ^ 
        (List.fold_left (fun acc course -> acc ^ (html_of_course course)) 
           "" sem.courses) ^ 
        "\t\t\t</tr>\n" end

  (** [html_of_schedule sch] is a string representation of [sch] in raw, 
      valid HTML form. *) 
  let html_of_schedule sch =
    match (get_sems sch) with
    | [] -> "<p>Schedule is empty!</p>\n"
    | _ -> begin
        "<h1><strong style=\"color:green;\">" ^ sch.desc ^ "</strong></h1>\n" ^ 
        "\t\t<h2>Cumulative GPA: <strong style=\"color:blue;\">" ^ 
        (gpa_to_string sch.cumul_gpa) ^ 
        "</strong></h2>\n" ^ 
        "\t\t<h2>Total Credits: <strong style=\"color:red;\">" ^ 
        (string_of_int sch.sch_credits) ^ "</strong></h2>\n" ^ 
        "\t\t<table>\n" ^ 
        (List.fold_left (fun acc sem -> acc ^ (html_of_sem sem)) 
           "" (get_sems sch)) ^ 
        "\t\t</table>\n" end

  (** [html_of_validation v] is a string that represents a validation displayed
      in valid HTML. *) 
  let html_of_validation (v_opt:validation option) = 
    match v_opt with
    | None -> ""
    | Some v ->
      "\t<h2>Testing Against CS " ^ v.sch ^ " Requirements:</h2>\n" ^ 
      "\t\t<ul>\n" ^
      (let req_course c =
         "\t\t\t<li><span>" ^
         "Missing Required Course: </span>" ^ c ^ "</li>\n"
       in
       List.fold_left (fun acc c -> acc ^ (req_course c)) "" v.needed) ^ 
      "\t\t</ul><ul>\n" ^
      (let required_cat cat =
         "\t\t\t<li><span>" ^ 
         "Not enough courses from category: </span>" ^ cat ^ "</li>\n"
       in
       List.fold_left 
         (fun acc (c,_) -> acc ^ (required_cat c)) "" v.needed_cat)^
      "\t\t</ul><ul>\n" ^
      (let required_subs c_lst =
         "\t\t\t<li><span>" ^ 
         "No course from required group: </span>" ^ 
         (string_of_list c_lst) ^ "</li>\n"
       in
       List.fold_left 
         (fun acc c -> acc ^ (required_subs c)) "" v.needed_subs) ^
      "\t\t</ul>\n"

  (** [save filename text] is [()] after saving [text] in file [filename]. *)
  let save filename text = 
    let chan = open_out filename in
    output_string chan text;
    close_out chan

  let export_schedule sch fl = 
    let reg1 = Str.regexp "<\\?sch_bg_color>" in
    let reg2 = Str.regexp "<\\?sch_square_color>" in
    let reg3 = Str.regexp "<\\?sch>" in
    let reg4 = Str.regexp "<\\?sch_validation>" in
    Str.global_replace reg1 sch.settings.html_background template
    |> Str.replace_first reg2 sch.settings.html_squares
    |> Str.replace_first reg3 (html_of_schedule sch)
    |> Str.replace_first reg4 (html_of_validation sch.valid)
    |> save fl

end

module LoadJSON = struct

  module Yj = struct include Yojson.Basic.Util end

  (** [form_sem_id semid] is the semester id formed from its string 
      representation [semid]. 
      Requires: [semid] is "None" or of form "FAYY" or "SPYY" where YY
      is a valid year code. *)
  let form_sem_id semid = 
    match Str.first_chars semid 2 with
    | "FA" -> let year = Str.last_chars semid 2 |> int_of_string in
      Fall year
    | "SP" -> let year = Str.last_chars semid 2 |> int_of_string in
      Spring year
    | "No" -> None
    | _ -> raise (UnknownSemester semid)

  (** [form_grade grade] is the grade represented by [grade]. *)
  let form_grade grade = 
    gradify grade

  (** [form_category cat] is the category represented by [cat]  *)
  let form_category cat =
    categorify cat

  (** [parse_course json] is a course generated from info found by 
      parsing [json]. *)
  let parse_course json = 
    {
      name = json |> Yj.member "name" |> Yj.to_string;
      credits = json |> Yj.member "course credits" |> Yj.to_int;
      grade = json |> Yj.member "grade" |> Yj.to_string |> form_grade;
      category = json |> Yj.member "category" |> Yj.to_string |> form_category;
    }

  (** [parse_semester json] is a semester generated from info found by 
      parsing [json] when schedule is for school [school]. *)
  let parse_semester json = 
    {
      id = json |> Yj.member "semester id" |> Yj.to_string |> form_sem_id;
      courses = json |> Yj.member "courses" |> Yj.to_list |> 
                List.map parse_course;
      tot_credits = json |> Yj.member "semester credits" |> Yj.to_int;
      sem_gpa = json |> Yj.member "semester gpa" |> Yj.to_float;
    }

  (** [parse_settings json] is a settings type generated from info found by 
      parsing [json]. *)
  let parse_settings json = 
    {
      autosave = json |> Yj.member "autosave" |> Yj.to_bool;
      html_background = json |> Yj.member "html_bg_color" |> Yj.to_string;
      html_squares = json |> Yj.member "html_square_color" |> Yj.to_string
    }

  let parse_json fl = 
    let json = Yojson.Basic.from_file fl in
    {
      desc = json |> Yj.member "description" |> Yj.to_string;
      semesters = json |> Yj.member "semesters" |> Yj.to_list |> 
                  List.map parse_semester;
      cumul_gpa = json |> Yj.member "cumul gpa" |> Yj.to_float;
      sch_credits = json |> Yj.member "sch credits" |> Yj.to_int;
      is_saved = true;
      settings = json |> Yj.member "settings" |> parse_settings;
      school = json |> Yj.member "school" |> Yj.to_string;
      valid = None
    }

end

module SaveJSON = struct

  (** [json_of_course c] is a string representation of [c] that can be 
      stored in a JSON file and later interpreted by Yojson.Basic *)
  let json_of_course c = 
    "\t\t\t\t{\n" ^
    "\t\t\t\t\t\"name\": \"" ^ c.name ^ "\",\n" ^
    "\t\t\t\t\t\"course credits\": " ^ (string_of_int c.credits) ^ ",\n" ^
    "\t\t\t\t\t\"grade\": \"" ^ (string_of_grade c.grade) ^ "\",\n" ^
    "\t\t\t\t\t\"category\": \"" ^ (string_of_category c.category) ^ "\"\n"^
    "\t\t\t\t},\n"

  (** [json_of_sem sem] is a string representation of [sem] that can be 
      stored in a JSON file and later interpreted by Yojson.Basic *)
  let json_of_sem sem = 
    let reg = Str.regexp "},\n$" in
    "\t\t{\n" ^
    "\t\t\t\"semester id\": \"" ^ (string_of_semid sem.id) ^ "\",\n" ^
    "\t\t\t\"semester credits\": " ^ (string_of_int sem.tot_credits) ^ ",\n" ^
    "\t\t\t\"semester gpa\": " ^ (gpa_to_string sem.sem_gpa) ^ ",\n" ^
    "\t\t\t\"courses\": [\n" ^ 
    (Str.replace_first reg "}\n" 
       (List.fold_left (fun acc course -> acc ^ (json_of_course course)) 
          "" (sem.courses))) ^
    "\t\t\t]\n\t\t},\n"

  (** [json_of_settigns set] is a string representation of [set] that can be 
      stored in a JSON file and later interpreted by Yojson.Basic *)
  let json_of_settings settings = 
    "{\n" ^
    "\t\t\t\"autosave\": " ^ (string_of_bool settings.autosave) ^ ",\n" ^
    "\t\t\t\"html_bg_color\": \"" ^ settings.html_background ^ "\",\n" ^
    "\t\t\t\"html_square_color\": \"" ^ settings.html_squares ^ "\"\n" ^
    "\t}"

  (** [json_of_schedule sch] is a string representation of [sch] that can be 
      stored in a JSON file and later interpreted by Yojson.Basic *)
  let json_of_schedule sch = 
    let reg = Str.regexp "},\n$" in
    "{\n" ^
    "\t\"description\": \"" ^ sch.desc ^ "\",\n" ^
    "\t\"cumul gpa\": "  ^ (gpa_to_string sch.cumul_gpa) ^ ",\n" ^
    "\t\"sch credits\": "  ^ (string_of_int sch.sch_credits) ^ ",\n" ^
    "\t\"settings\": " ^ (json_of_settings sch.settings) ^ ",\n" ^
    "\t\"school\": \"" ^ sch.school ^ "\",\n" ^
    "\t\"semesters\": [\n" ^ 
    (Str.replace_first reg "}\n" 
       (List.fold_left (fun acc sem -> acc ^ (json_of_sem sem)) 
          "" (sch.semesters))) ^
    "\t]\n}\n"

  let save_schedule sch fl =
    let chan = open_out fl in
    output_string chan (json_of_schedule sch);
    sch.is_saved <- true;
    close_out chan

end

let autosave sch = 
  if sch.settings.autosave && (not sch.is_saved) then
    SaveJSON.save_schedule sch (sch.desc ^ ".json")
  else ()