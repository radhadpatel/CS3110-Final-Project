open Schedule
open Command
open ClassRoster

(** [valid_commands] is a textual representation of a list of valid commands
    the user can enter in [prompt] *)
let valid_commands = 
  ("Valid Commands: add | edit | remove | swap | move | check | save |" ^
   " set | print | import | export | close | quit")

(** [read_input ()] is [read_line ()] after displaying helpful text prompt
    indicator. *)
let read_input () = 
  print_string "\n> ";
  read_line ()

(** [check_last_word str_lst] is the tuple with the first element as schedule 
    name that is not only whitespaces and the second element as the school 
    ("ENG" or "CAS"). Tuple has both elements as empty strings if the schedule 
    name is the empty string.
    Raises: [UnknownSchool str] if school is not "ENG" nor "CAS". *)
let rec check_last_word str_lst str_acc = 
  match str_lst with
  | [] -> ("", "")
  | last::[] -> let upper_last = String.uppercase_ascii last in
    let trimmed_str = String.trim str_acc in
    if trimmed_str = "" then ("", "")
    else if upper_last = "CAS" then (trimmed_str, "CAS") 
    else if upper_last = "ENG" then (trimmed_str, "ENG") 
    else raise (UnknownSchool upper_last)
  | h::t -> check_last_word t (str_acc ^ h ^ " ")

(** [save_prompt_helper sch] is [sch] after saving [sch] as a json file. *)
let save_prompt_helper sch =
  print_endline 
    ("\nChoose the name of the JSON file you will save this schedule to:");
  save_handler sch (String.split_on_char ' ' (read_input ()))

(** [save_prompt_from_quit sch] prompts the user whether they want to save
    [sch] or not after the Quit command. *)
let rec save_prompt_from_quit sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before quitting?";
  match read_input () with
  | "cancel" -> prompt sch
  | "yes" -> ignore(save_prompt_helper sch); set_save_status sch true;
    start_prompt ()
  | "no" -> Stdlib.exit 0
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue. 'cancel' to undo this " ^
                   "command."); 
    save_prompt_from_quit sch

(** [save_prompt_from_close sch] prompts the user whether they want to save 
    [sch] or not after the Close command. *)
and save_prompt_from_close sch =
  ANSITerminal.print_string [Bold] "Warning: ";
  print_endline 
    "You have unsaved changes. Would you like to save now before closing?";
  match read_input () with
  | "cancel" -> prompt sch
  | "yes" -> ignore(save_prompt_helper sch); set_save_status sch true; 
    start_prompt ()
  | "no" -> start_prompt ()
  | _ -> 
    print_endline ("Type 'yes' or 'no' to continue. 'cancel' to undo this " ^
                   "command."); 
    save_prompt_from_close sch

(** [prompt sch] is the user's interface with our system. This function handles 
    execution of user commands pertaining to [sch]. Also handles any exceptions 
    raised during the execution of any commands. *)
and prompt sch =
  ANSITerminal.(print_string [green] ("\n" ^ (get_name sch) ^ ": "));
  match read_line () with 
  | exception End_of_file -> ()
  | "quit" -> 
    if get_save_status sch then Stdlib.exit 0 
    else save_prompt_from_quit sch
  | "close" -> 
    if get_save_status sch then start_prompt ()
    else save_prompt_from_close sch; init_prompt ()
  | "" -> begin
      print_endline valid_commands;
      print_endline "Enter a command to view usage instructions.";
      prompt sch
    end
  | string_cmd ->
    try
      prompt (parse_command sch string_cmd)
    with
    | UnknownCourse msg -> 
      exceptions sch ("Invalid/Unknown Course: " ^ msg)
    | UnknownSemester msg -> 
      exceptions sch ("Invalid/Unknown Semester: " ^ msg)
    | UnknownGrade msg -> 
      exceptions sch ("Invalid/Unknown Grade: " ^ msg ^ 
                      "\nValid grades: <letter_grade>, s/sat, u/unsat, "^
                      "w/withdrawn, inc/incomplete, none, transfer")
    | UnknownCategoryENG msg ->
      exceptions sch ("Invalid/Unknown ENG Category: " ^ msg ^
                      "\nValid ENG categories: PE, FWS, req/required, core, " ^ 
                      "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                      "project/proj/practicum/pract, ENGRD, ENGRI, liberal, "^
                      "aprv/advisor, extra")
    | UnknownCategoryCAS msg ->
      exceptions sch ("Invalid/Unknown CAS Category: " ^ msg ^
                      "\nValid CAS categories: PE, FWS, req/required, core, " ^ 
                      "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                      "project/proj/practicum/pract, lang/language/foreign, "^
                      "PBS/PBSS, MQR/CA/HA/KCM/LA/SBA/liberal, GB, HB, "^
                      "extra")
    | UnknownSetting msg ->
      exceptions sch ("Invalid/Unknown Setting Attribute: " ^ msg ^
                      "\nValid attributes: autosave, html_bg_color, " ^
                      "html_tile_color")
    | DuplicateCourse msg -> 
      exceptions sch ("Duplicate Course Already Exists: " ^ msg)
    | DuplicateSemester msg -> 
      exceptions sch ("Duplicate Semester Already Exists: " ^ msg)
    | InvalidAttribute msg ->
      exceptions sch ("Invalid/Unknown Edit Attribute: " ^ msg ^
                      "\nValid attributes: credits, grade, or category.")
    | InvalidSwap ->
      exceptions sch 
        "Cannot swap course with itself or courses that are in same semester."
    | InvalidMove ->
      exceptions sch
        "This course is already located in this semester."
    | ClassRoster.InvalidURL -> 
      exceptions sch "Error Retrieving Course Info from Online"
    | InvalidFileForExport ->
      exceptions sch "File path cannot be a JSON. Try again."
    | ICalParser.InvalidFileForImport ->
      exceptions sch "File must be an iCal in current directory. Try again."
    | InvalidFileForSave -> 
      exceptions sch "File path must be a JSON in current directory. Try again." 
    | SemesterDoesNotExist ->
      exceptions sch "This semester does not exist in your schedule."
    | MalformedSemId -> 
      exceptions sch ("Incorrect Semester Entry Format: " ^
                      "Eg; use 'fa18' for fall 2018 and 'sp22' for spring 2022")
    | MalformedAdd ->
      if get_school sch = "ENG" 
      then
        exceptions sch ("Usage: add [<course_name> (optional: <credits>) "^
                        "<grade> (optional: <category>) <semester> | "^
                        "<semester>]\nValid grades: <letter_grade>, s/sat, "^
                        "u/unsat, w/withdrawn, inc/incomplete, none, transfer" ^
                        "\nValid ENG categories: PE, FWS, req/required, core, "^ 
                        "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                        "project/proj/practicum/pract, ENGRD, ENGRI, liberal, "^
                        "aprv/advisor, extra")
      else
        exceptions sch ("Usage: add [<course_name> (optional: <credits>) "^
                        "<grade> (optional: <category>) <semester> | "^
                        "<semester>]\nValid grades: <letter_grade>, s/sat, "^
                        "u/unsat, w/withdrawn, inc/incomplete, none, transfer" ^
                        "\nValid CAS categories: PE, FWS, req/required, core, "^ 
                        "4000+, tech/technical, spcl/ext, maj/major, " ^ 
                        "project/proj/practicum/pract, lang/language/foreign, "^
                        "PBS/PBSS, MQR/CA/HA/KCM/LA/SBA/liberal, GB, HB, "^
                        "extra")
    | MalformedEdit ->
      exceptions sch ("Usage: edit [<course_name> <field> <new_value> | " ^ 
                      "name <new_name> | school [ENG | CAS] ]" ^
                      "\nValid fields: credits, grade, category.")
    | MalformedRemove ->
      exceptions sch "Usage: remove [<course_name> | <semester>]"
    | MalformedExport ->
      exceptions sch "Usage: export <html_file_name>"
    | MalformedSave ->
      exceptions sch "Usage: save <json_file>"
    | MalformedSwap -> 
      exceptions sch "Usage: swap <course_name> <course_name>"
    | MalformedMove ->
      exceptions sch "Usage: move <course_name> <new_semester>"
    | MalformedPrint ->
      exceptions sch "Usage: print [<> | <course_name>]"
    | MalformedSet ->
      exceptions sch ("Usage: set <attribute> <new_value>" ^
                      "\nValid attributes: autosave, html_bg_color, " ^
                      "html_tile_color")
    | MalformedImport -> 
      exceptions sch "Usage: import <ics_file>"
    | Malformed | Empty -> 
      exceptions sch 
        ("Unrecognized Command Entry!\n" ^ valid_commands)

(** [exceptions sch err] is [prompt sch] after printing error message for 
    [err]. *)
and exceptions sch err = 
  ANSITerminal.(print_string [red] "Invalid\n"); 
  print_endline err;
  prompt sch

(** [load file_lst] is [prompt sch] where [sch] was parsed from JSON file 
    [file_list]. If [file_list] doesn't point to a valid JSON schedule file, 
    [load file_lst] is [init_pompt ()]. *)
and load (file_lst: string list) =
  try 
    let file_extra_space = 
      List.fold_left (fun acc str -> acc ^ str ^ " ") "" file_lst in
    let file = 
      String.sub file_extra_space 0 (String.length file_extra_space - 1) in
    let sch = LoadJSON.parse_json file in
    prompt sch
  with
  | _ -> print_string ("\nInvalid/Unknown JSON file.\n"); init_prompt ()

(** [init_prompt ()] is the initial user prompt and first entry into the 
    system. *)
and init_prompt () =
  let split_cmd = String.split_on_char ' ' (read_input ()) in
  match split_cmd with 
  | [] -> raise Empty
  | "new"::sch_name_and_school -> begin
      try
        let (sch_name, school) = 
          if List.length sch_name_and_school < 2 
          then ("", "") 
          else check_last_word sch_name_and_school "" in
        if sch_name <> "" then 
          begin
            print_endline("\nThe following commands are available for use. Type" 
                          ^ " in any command to see usage instructions.");
            ANSITerminal.(print_string [yellow] valid_commands);
            print_newline ();
            prompt (new_schedule sch_name school)
          end
        else 
          begin
            ANSITerminal.(print_string [red] 
                            ("Schedule name cannot contain only"
                             ^" whitespaces!\n"));
            print_endline 
              ("Valid commands: [new <schedule_name> <school>] | " ^
               "[load <json_file>] | quit");
            init_prompt ()
          end
      with
      | _ -> ANSITerminal.(print_string [red] ("Not a valid school!\n"));
        print_endline
          "Valid schools: eng/ENG, cas/CAS"; 
        init_prompt ()
    end
  | "load"::json_lst when json_lst <> [] -> load json_lst
  | "quit"::[] -> Stdlib.exit 0
  | _ -> 
    ANSITerminal.(print_string [red] "\nUnrecognized Command Entry!\n");
    print_endline 
      ("Valid commands: [new <schedule_name> <school>] | " ^
       "[load <json_file>] | quit");
    init_prompt ()

(** [start_prompt ()] is the low-level prompt the user is taken to when not
    working within an active schedule. *)
and start_prompt () =
  ANSITerminal.(print_string [cyan] "\nStart Page\n"); 
  print_endline 
    ("Valid commands: [new <schedule_name> <school>] | " ^
     "[load <json_file>] | quit");
  init_prompt ()

let main () =
  ignore(Sys.command "clear");
  ANSITerminal.(print_string [red]
                  "Welcome to the 3110 Project Schedule Planning Tool\n");
  print_endline 
    "If you want to open an already existing schedule, type [load <json_file>]";
  print_endline 
    "Or type [new <schedule_name> <school>] to create a new schedule.";
  print_endline
    "Valid schools: eng/ENG, cas/CAS";
  init_prompt ()

(* Starts system *)
let () = main ()