open Schedule
open ClassRoster

exception Empty

exception Malformed
exception MalformedSemId
exception MalformedAdd
exception MalformedEdit
exception MalformedRemove
exception MalformedSwap
exception MalformedMove
exception MalformedSave
exception MalformedExport
exception MalformedImport
exception MalformedPrint
exception MalformedSet

exception InvalidFileForExport
exception InvalidFileForSave

exception SemesterDoesNotExist

(** [is_valid_coursename str] is [true] if [str] has the correct format of a 
    Cornell class. *)
let is_valid_coursename str =
  (Str.string_match (Str.regexp "^[A-Z][A-Z]+[0-9][0-9][0-9][0-9]$") str 0) 

(** [sem_id_parse semid] is [sem_id] where [semid] is the string 
    representation of [sem_id]. 
    Raises: [MalformedSemId] if [semid] is not a proper string rep. *)
let sem_id_parse sem_id =
  let uppercase_id = String.uppercase_ascii sem_id in
  if Str.string_match (Str.regexp "^SP[0-9][0-9]$") uppercase_id 0 then
    Spring (int_of_string (String.sub sem_id 2 2))
  else if Str.string_match (Str.regexp "^FA[0-9][0-9]$") uppercase_id 0 then 
    Fall (int_of_string (String.sub sem_id 2 2)) 
  else 
    raise MalformedSemId

(** [sem_exists sem_id_list sem_id] is [()] if [sem_id] is in [sem_id_list]. 
    Rasies [SemesterDoesNotExist] otherwise. *)
let rec sem_exists sem_id_lst sem_id =
  match sem_id_lst with 
  | [] -> raise SemesterDoesNotExist
  | sem::others when sem = (String.uppercase_ascii sem_id) -> ()
  | sem::others -> sem_exists others sem_id

(** [format_sem_id str] is [true] if [str] conforms to a string representation
    of a semester id. *)
let format_sem_id str =
  let id = String.uppercase_ascii(String.sub str 0 2) in
  (id = "SP" || id = "FA")

(** [core_check c_name] is [true] if [c_name] is a core class,
    otherwise [false]. *)
let core_check c_name =
  c_name = "CS2800" || c_name = "CS2802" || c_name = "CS3110" || 
  c_name = "CS3410" || c_name = "CS3420" || c_name = "ECE3140" || 
  c_name = "CS4410" || c_name = "CS4280"                     

(** [technical_check c_name] is [true] if [c_name] is a technical elective in 
    either ENG or CAS, otherwise [false]. *)
let technical_check c_name =
  String.sub c_name 0 5 = "ECON3" || String.sub c_name 0 5 = "ECON4" 
  || String.sub c_name 0 5 = "ECON5" || String.sub c_name 0 5 = "MATH3" 
  || String.sub c_name 0 5 = "MATH4" || String.sub c_name 0 5 = "MATH5"
  || String.sub c_name 0 5 = "CHEM3" || String.sub c_name 0 5 = "CHEM4"
  || String.sub c_name 0 5 = "CHEM5" || String.sub c_name 0 6 = "BIOEE3"
  || String.sub c_name 0 6 = "BIOEE4" || String.sub c_name 0 3 = "CS3" 
  || String.sub c_name 0 6 = "BIOEE5"

(** [practicum_check c_name] is [true] if [c_name] is a CS practicum course, 
    otherwise [false]. *)
let practicum_check c_name =
  String.sub c_name ((String.length c_name) - 1) 1 = "1"
  && String.sub c_name 0 3 = "CS4"

(** [requiredENG_check c_name] is [true] if [c_name] is a required course 
    for ENG, otherwise [false]. *)
let requiredENG_check c_name = 
  c_name = "CS1110" || c_name = "CS1112" || c_name = "MATH1910" ||
  c_name = "MATH1920" || c_name = "MATH2940" || c_name = "CHEM2090" ||
  c_name = "CHEM2080" || c_name = "CHEM2150" || c_name = "BTRY3080" || 
  c_name = "ECON3130" || c_name = "MATH2930" || c_name = "MATH4710" || 
  c_name = "PHYS2214" || c_name = "PHYS2218" || c_name = "PHYS1112" ||
  c_name = "PHYS116" || c_name = "PHYS2213" || c_name = "PHYS2217" || 
  c_name = "CS4850" || c_name = "ECE3100" || c_name = "ENGRD2700"

(** [four_thousand_plus_check c_name] is [true] if [c_name] is a CS4000+ class 
    exlcuding CS4090, CS4998, and CS4999. Otherwise, [false]. *)
let four_thousand_plus_check c_name = 
  (String.sub c_name 0 3 = "CS4" && (c_name <> "CS4090") 
   && (c_name <> "CS4998") && (c_name <> "CS4999")) 
  || String.sub c_name 0 3 = "CS5"

(** [guess_catENG c_name sem_id] is the calculated estimate of a c_name's 
    category based on c_name in ENG. *)
let guess_catENG c_name sem_id = 
  if String.sub c_name 0 2 = "PE" then PE
  else if get_FWS_status c_name sem_id then FWS
  else if String.sub c_name 0 5 = "ENGRI" then ENGRI
  else if String.sub c_name 0 5 = "ENGRD" || c_name = "CS2110" || 
          c_name = "CS2112" then ENGRD
  else if practicum_check c_name then Practicum
  else if requiredENG_check c_name then Required
  else if core_check c_name then Core
  else if four_thousand_plus_check c_name then FourThousandPlus
  else if technical_check c_name then Technical
  else if not(Str.string_match 
                (Str.regexp "^[A-Z][A-Z]+[0-2][0-9][0-9][0-9]$") c_name 0)
  then Specialization
  else Liberal

(** [language_check c_name] is [true] if [c_name] is a foreign language,
    otherwise [false]. *)
let language_check c_name = 
  String.sub c_name 0 4 = "ARAB" || String.sub c_name 0 3 = "BCS" ||
  String.sub c_name 0 5 = "BENGL" || String.sub c_name 0 4 = "BURM" || 
  String.sub c_name 0 4 = "CHIN" || String.sub c_name 0 4 = "CZECH" ||
  String.sub c_name 0 5 = "DUTCH" || String.sub c_name 0 4 = "FINN" ||
  String.sub c_name 0 4 = "FREN" || String.sub c_name 0 5 = "GREEK" ||
  String.sub c_name 0 5 = "HEBRW" || String.sub c_name 0 5 = "HINDI" ||
  String.sub c_name 0 5 = "HUNGR" || String.sub c_name 0 4 = "INDO" ||
  String.sub c_name 0 4 = "ITAL" || String.sub c_name 0 5 = "JAPAN" ||
  String.sub c_name 0 5 = "KOREA" || String.sub c_name 0 5 = "NEPAL" ||
  String.sub c_name 0 5 = "PERSN" || String.sub c_name 0 5 = "POLSH" ||
  String.sub c_name 0 4 = "PORT" || String.sub c_name 0 5 = "PUNJB" ||
  String.sub c_name 0 5 = "ROMAN" || String.sub c_name 0 4 = "SPAN" ||
  String.sub c_name 0 4 = "TURK" || String.sub c_name 0 5 = "UKRAN" ||
  String.sub c_name 0 4 = "VIET"

(** [requiredCAS_check c_name] is [true] if [c_name] is a required course 
    for CAS, otherwise [false]. *)
let requiredCAS_check c_name = 
  c_name = "CS1110" || c_name = "CS1112" || c_name = "MATH1910" ||
  c_name = "MATH1920" || c_name = "MATH2940" || c_name = "CS4850" ||
  c_name = "ECE3100" || c_name = "ECON3130" || c_name = "BTRY3080" || 
  c_name = "ENGRD2700" || c_name = "MATH4710" || c_name = "MATH1110" || 
  c_name = "MATH1120" || c_name = "MATH1220" || c_name = "MATH2210"

(** [guess_catCAS c_name sem_id] is the calculated estimate of a c_name's 
    category based on c_name in CAS. *)
let guess_catCAS c_name sem_id =
  let distribution = distribution_category c_name sem_id in
  if String.sub c_name 0 2 = "PE" then PE
  else if get_FWS_status c_name sem_id then FWS 
  else if practicum_check c_name then Practicum
  else if requiredCAS_check c_name then Required
  else if core_check c_name then Core
  else if four_thousand_plus_check c_name then FourThousandPlus
  else if technical_check c_name || c_name = "MATH2930" then Technical
  else if language_check c_name then ForeignLanguage
  else if breadth_category c_name sem_id = "(GB)" then GB
  else if breadth_category c_name sem_id = "(HB)" then HB
  else if breadth_category c_name sem_id = "(GHB)" then GB
  else if distribution = "(CA-AS)" then Liberal
  else if distribution = "(HA-AS)" then Liberal
  else if distribution = "(KCM-AS)" then Liberal
  else if distribution = "(LA-AS)" then Liberal
  else if distribution = "(SBA-AS)" then Liberal
  else if distribution = "(PBS-AS)" then PBS
  else if distribution = "(PBSS-AS)" then PBS
  else Specialization

(** [add_others sch str_lst] is [sch] after parsing [str_lst] and adding a 
    new course if [str_lst] is properly formatted. 
    Raises: [MalformedAdd] if [str_lst] not properly formatted. *)
let add_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedAdd
  | sem_id::[] when format_sem_id sem_id -> 
    let sch' = add_sem sch (create_sem (sem_id_parse sem_id)) in
    print_endline("Added " ^ String.uppercase_ascii sem_id); sch'
  | course_name::grade::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    let guessed_cat = if get_school sch = "ENG" 
      then guess_catENG name (sem_id_parse sem_id) 
      else guess_catCAS name (sem_id_parse sem_id) in
    let sch' = add_course sch 
        (create_course name 
           (get_course_creds name 
              (sem_id_parse sem_id)) 
           (Schedule.gradify grade) (guessed_cat)) 
        (sem_id_parse sem_id) 
    in print_endline("Category Estimation: " ^ 
                     (string_of_category guessed_cat));
    print_endline("Added " ^ name); sch' 
  | course_name::credits::grade::sem_id::[] 
    when Str.string_match (Str.regexp "^[0-9]+$") credits 0 ->
    (sem_exists (sem_ids_to_string sch) sem_id); 
    let name = String.uppercase_ascii course_name in
    let guessed_cat = if get_school sch = "ENG" 
      then guess_catENG name (sem_id_parse sem_id) 
      else guess_catCAS name (sem_id_parse sem_id) in

    let sch' = add_course sch (create_course name
                                 (int_of_string credits)
                                 (Schedule.gradify grade) guessed_cat)
        (sem_id_parse sem_id)
    in print_endline ("Category Estimation: " ^ 
                      (string_of_category guessed_cat)); 
    print_endline("Added " ^ name); sch'
  | course_name::grade::category::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    let sch' = add_course sch 
        (create_course name 
           (get_course_creds name 
              (sem_id_parse sem_id)) 
           (gradify grade) (String.uppercase_ascii category |> categorify)) 
        (sem_id_parse sem_id)
    in print_endline("Added " ^ name); sch'
  | course_name::credits::grade::category::sem_id::[] ->
    (sem_exists (sem_ids_to_string sch) sem_id);
    let name = String.uppercase_ascii course_name in
    let sch' = add_course sch 
        (create_course name 
           (int_of_string credits) 
           (gradify grade) 
           (String.uppercase_ascii category |> categorify)) 
        (sem_id_parse sem_id)
    in print_endline("Added " ^ name); sch'
  | _ -> raise MalformedAdd

(** [add_others sch str_lst] is [sch] after parsing [str_lst] and editing a 
    course value if [str_lst] is properly formatted. 
    Raises: [MalformedEdit] if [str_lst] not properly formatted. *)
let edit_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedEdit
  | "name"::new_val::[] -> edit_name sch new_val
  | "school"::school::[] -> begin
      let school' = (String.uppercase_ascii school) in
      ignore(check_school school');
      set_school sch school'; sch
    end
  | course_name::field::new_val::[] ->
    edit_course sch (String.uppercase_ascii course_name) field new_val
  | _ -> raise MalformedEdit

(** [remove_others sch str_lst] is [sch] after parsing [str_lst] and removing a 
    new course if [str_lst] is properly formatted. 
    Raises: [MalformedRemove] if [str_lst] not properly formatted. *)
let remove_others sch str_lst =
  match str_lst with
  | [] -> raise MalformedRemove
  | sem_id::[] when format_sem_id sem_id && is_valid_coursename sem_id = false-> 
    let sch' = remove_sem sch (sem_id_parse sem_id) in 
    print_endline("Removed " ^ String.uppercase_ascii sem_id); sch'
  | course_name::[] -> 
    let upper_course = String.uppercase_ascii course_name in
    let sch' = remove_course sch upper_course in 
    print_endline("Removed " ^ upper_course); sch'
  | _ -> raise MalformedRemove

(** [is_not_json file] is [false] if [file] has the extension ".json".
    Raises: [InvalidFileForExport] if [file] is a .json. *)
let is_not_json file =
  match String.split_on_char '.' file with
  | name::extension::[] when extension <> "json" -> true
  | _ -> raise InvalidFileForExport

(** [export_handler sch str_lst] is [sch] after parsing [str_lst] and exporting
    [sch] as HTML file if [str_lst] is properly formatted. 
    Raises: [MalformedExport] if [str_lst] not properly formatted.
    Raises: [InvalidFileForExport] if filenmae given in [str_lst] isnt valid. *)
let export_handler sch str_lst = 
  (if get_school sch = "ENG" then
     ignore (Requirements.validate sch Requirements.eng_reqs)
   else
     ignore (Requirements.validate sch Requirements.cas_reqs));
  match str_lst with
  | file :: [] -> print_endline("Trying to export to " ^ file ^ "...");
    if is_not_json file
    then (HTML.export_schedule sch file; 
          ANSITerminal.print_string [Bold] "\nSuccessfully exported!\n"; sch) 
    else raise InvalidFileForExport
  | _ -> raise MalformedExport

(** [import_handler sch str_lst] is [sch] after parsing [str_lst] and importing
    an iCal file if [str_lst] is properly formatted. 
    Raises: [MalformedImport] if [str_lst] not properly formatted.
    Raises: [InvalidFileForImport] if filenmae given in [str_lst] isnt valid. *)
let import_handler sch str_lst = 
  match str_lst with
  | file :: [] -> print_endline("Trying to import " ^ file ^ "...");
    begin
      let courses, sem_id = 
        try ICalParser.parse_file file 
        with Sys_error _ -> 
          raise ICalParser.InvalidFileForImport 
      in 
      let sch' = 
        try (sem_exists (sem_ids_to_string sch) sem_id); sch
        with SemesterDoesNotExist -> 
          add_sem sch (create_sem (sem_id_parse sem_id))
      in
      let sch'' = List.fold_left 
          (fun acc name -> 
             try add_course acc 
                   (create_course name
                      (get_course_creds name 
                         (sem_id_parse sem_id))
                      (gradify "none") 
                      (if get_school sch = "ENG" 
                       then guess_catENG name (sem_id_parse sem_id) 
                       else guess_catCAS name (sem_id_parse sem_id))) 
                   (sem_id_parse sem_id) 
             with DuplicateCourse _ -> acc) sch' courses 
      in ANSITerminal.print_string [Bold] "\nSuccessfully imported!\n";
      sch''
    end
  | _ -> raise MalformedImport

(** [is_json file] is [true] if [file] has the extension .json.
    Raises: InvalidFileForSave if [file] is not a .json. *)
let is_json file =
  match String.split_on_char '.' file with
  | name::extension::[] when name <> "" && extension = "json" -> true
  | _ -> raise InvalidFileForSave

(** [save_handler sch str_lst] is [sch] after parsing [str_lst] and saving [sch]
    as JSON file if [str_lst] is properly formatted. 
    Raises: [MalformedSave] if [str_lst] not properly formatted.
    Raises: [InvalidFileForSave] if filenmae given in [str_lst] isnt valid. *)
let save_handler sch str_lst = 
  match str_lst with
  | file :: [] -> if is_json file 
    then (SaveJSON.save_schedule sch file;  
          ANSITerminal.print_string [Bold] "\nSaved!\n"; sch) 
    else raise InvalidFileForSave
  | _ -> raise MalformedSave

(** [swap_others sch str_lst] is [sch] after parsing [str_lst] and swapping
    courses between semesters if [str_lst] is properly formatted. 
    Raises: [MalformedSwap] if [str_lst] not properly formatted. *)
let swap_others sch str_lst =
  match str_lst with
  | course1::course2::[] -> 
    let upper_course1 = String.uppercase_ascii course1 in
    let upper_course2 = String.uppercase_ascii course2 in
    let sch' = swap_courses upper_course1 upper_course2 sch in
    print_endline(upper_course1 ^ " swapped with " ^ upper_course2); sch'
  | _ -> raise MalformedSwap

(** [move_others sch str_lst] is [sch] after parsing [str_lst] and moving course
    to a new semester if [str_lst] is properly formatted. 
    Raises: [MalformedMove] if [str_lst] not properly formatted. *)
let move_others sch str_lst =
  match str_lst with
  | course::sem::[] -> (sem_exists (sem_ids_to_string sch) sem); 
    let upper_course = String.uppercase_ascii course in
    let sch' = move_course upper_course (sem_id_parse sem) sch in
    print_endline(upper_course ^ " moved to " ^ String.uppercase_ascii sem); 
    sch'
  | _ -> raise MalformedMove

(** [settings_handler sch str_lst] is [sch] after parsing [str_lst] and changing
    settings in [sch] if [str_lst] is properly formatted. 
    Raises: [MalformedSet] if [str_lst] not properly formatted. *)
let settings_handler sch str_lst = 
  match str_lst with
  | attr::new_val::[] -> edit_settings sch attr new_val
  | _ -> raise MalformedSet

(** [validate_handler sch] is [sch] after chekcing updating validity of 
    schedule. *)
let validate_handler sch = 
  if get_school sch = "ENG" then
    (Requirements.validate sch Requirements.eng_reqs
     |> Requirements.print_validation;
     sch)
  else
    (Requirements.validate sch Requirements.cas_reqs
     |> Requirements.print_validation;
     sch)


let parse_command sch cmd_str = 

  let match_helper first others =
    match first with
    | "add" -> add_others sch others
    | "edit" -> edit_others sch others
    | "remove" -> remove_others sch others
    | "swap" -> swap_others sch others
    | "move" -> move_others sch others
    | "save" -> save_handler sch others
    | "export" -> export_handler sch others
    | "import" -> import_handler sch others
    | "set" -> settings_handler sch others
    | "check" -> validate_handler sch
    | _ -> raise Malformed
  in

  let split_cmd = String.split_on_char ' ' cmd_str in
  match split_cmd with 
  | [] -> raise Empty
  | "print"::[] -> print_schedule sch; sch
  | "print"::c::[] ->
    let nm = String.uppercase_ascii c in
    if is_valid_coursename nm then
      (get_course nm (to_list sch) |> print_course sch; sch)
    else
      raise MalformedPrint
  | fst::others -> 
    let new_sch = match_helper fst others in
    autosave new_sch; new_sch