(** TEST PLAN: 

    We tested the functions in the Schedule, LoadJSON, SaveJSON, iCalParser, 
    and classRoster modules using OUnit testing because these were the basic 
    functions. For the rest of the code (main.ml, command.ml, requirements.ml),
    we used manual/interactive testing. Obviously, this could only be done once
    main.ml was funcitonal and "make run" worked. 

    Manual testing made sure loading and saving JSON files worked, exporting
    to HTML worked, the settings worked, and the basic functionality. After
    MS1, our system was more complicated and simpler to test manually by
    running it. Manual testing was better for checking if courses and semesters
    were being added, edited, and removed correctly because a lot of our fields
    were mutable -- which made them difficult to test with OUnit cases.

    Most of the testing was done after the function being tested was written,
    however, the person testing the function was not the same person who
    wrote it, therefore, most of the testing was black-box. The testing
    was based off of functions in schedule.mli and their documentation and 
    not based on the implementation of the function.

    We believe that (primarily) manual testing was a sufficient strategy for
    this system because this is an interactive tool, and we felt the best way
    to ensure its success was to thoroughly test it as the user would use it.
*)

open OUnit2
open Schedule

(** [string_of_sch sch] is a string representation of [sch] astisfying that 
    each unique [sch] has a unique string representation via this function
    (more or less). *)
let string_of_sch sch =
  let courses = to_list sch in
  List.fold_left (fun acc course -> acc ^ get_course_name course) "" courses

(** [make_int_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for ints. *)
let make_int_test
    (name: string)
    (expected_output: int) 
    (actual_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_int)

(** [make_bool_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for booleans. *)
let make_bool_test
    (name: string)
    (expected_output: bool) 
    (actual_output: bool) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_bool)

(** [make_float_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for floats. *)
let make_float_test
    (name: string)
    (expected_output: float) 
    (actual_output: float) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:string_of_float)              

(** [make_string_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for strings. *)
let make_string_test
    (name: string)
    (expected_output: string) 
    (actual_output: string) : test = 
  name >:: (fun _ -> assert_equal expected_output actual_output
               ~printer:(fun x -> x)) 

(** [make_list_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for lists. *)
let make_list_test
    (name: string)
    (expected_output: string list)
    (actual_output: string list) : test = 
  name >:: (fun _ -> assert_equal (List.sort_uniq compare expected_output)
               (List.sort_uniq compare actual_output)
               ~printer:Schedule.string_of_list)   

(** [make_exn_test name expected_output actual_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [actual_output] for exceptions. 
    let make_exn_test
    (name: string)
    (expected_output: exn)
    (actual_output: schedule) : test =
    name >:: (fun _ -> assert_raises expected_output 
               (fun () -> actual_output))*)

(** [make_add_sem_test name sch sem expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [add_sem sch sem]. *)
let make_add_sem_test
    (name: string)
    (sch: schedule)
    (sem: semester)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output (fun () -> add_sem sch sem))

(** [make_rem_sem_test name sch semid expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [rem_sem sch semid]. *)
let make_rem_sem_test
    (name: string)
    (sch: schedule)
    (semid: sem_id)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> remove_sem sch semid))

(** [make_add_course_test name sch course semid expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with
    [add_course sch course semid]. *)
let make_add_course_test
    (name: string)
    (sch: schedule)
    (course: course)
    (semid: sem_id)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> add_course sch course semid))

(** [make_rem_course_test name sch cname expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [rem_course sch cname]. *)
let make_rem_course_test
    (name: string)
    (sch: schedule)
    (cname: string)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> remove_course sch cname))

(** [make_edit_course_test name sch cname attr new_val expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [edit_course sch cname attr new_val]. *)
let make_edit_course_test
    (name: string)
    (sch: schedule)
    (cname: string)
    (attr: string)
    (new_val: string)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> edit_course sch cname attr new_val))

(** [make_swap_courses_test name sch cname1 cname2 expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [swap_courses cname1 cname2 sch]. *)
let make_swap_courses_test
    (name: string)
    (sch: schedule)
    (cname1: string)
    (cname2: string)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> swap_courses cname1 cname2 sch))

(** [make_move_course_test name sch cname semid expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [move_course cname semid sch]. *)
let make_move_course_test
    (name: string)
    (sch: schedule)
    (cname: string)
    (semid: sem_id)
    (expected_output: exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> move_course cname semid sch))

(* 
    Code used in all test cases:
*)

let fall19 = create_sem (Fall 19)
let sp20 = create_sem (Spring 20)

let cs2800 = create_course "CS2800" 3 (Letter "C") Core
let cs4820 = create_course "CS4820" 4 (Letter "C+") Core
let phys2213 = create_course "PHYS2213" 4 (Letter "A-") Required
let cs3110 = create_course "CS3110" 4 (Letter "B") Core
let btry3080 = create_course "BTRY3080" 4 (Letter "A") Required

(*
    Schedule module tests
*)

let sch = new_schedule "Sch1" "ENG"
let _ = add_sem sch fall19
let _ = add_sem sch sp20
let _ = add_course sch cs2800 (Fall 19)
let _ = add_course sch cs4820 (Spring 20)
let _ = add_course sch cs3110 (Fall 19)
let _ = add_course sch phys2213 (Spring 20)

let sch_rem_sem = new_schedule "Sch2" "ENG"
let _ = add_sem sch_rem_sem (create_sem (Fall 19))
let _ = add_sem sch_rem_sem (create_sem (Spring 20))
let _ = remove_sem sch_rem_sem (Fall 19)
let _ = add_course sch_rem_sem cs2800 (Spring 20)
let _ = add_course sch_rem_sem cs3110 (Spring 20)
let _ = add_course sch_rem_sem cs4820 (Spring 20)
let _ = remove_course sch_rem_sem "CS2800"

let edit_creds_sch = new_schedule "Sch3" "ENG"
let _ = add_sem edit_creds_sch (create_sem (Fall 20))
let _ = add_course edit_creds_sch phys2213 (Fall 20)
let _ = edit_course edit_creds_sch "PHYS2213" "credits" "3"

let edit_grade = new_schedule "Sch4" "ENG"
let _ = add_sem edit_grade (create_sem (Fall 20))
let _ = add_course edit_grade phys2213 (Fall 20)
let _ = edit_course edit_grade "PHYS2213" "grade" "B"

let unswapped = new_schedule "Sch5" "CAS"
let _ = add_sem unswapped (create_sem (Fall 20))
let _ = add_sem unswapped (create_sem (Spring 21))
let _ = add_course unswapped cs2800 (Spring 21)
let _ = add_course unswapped cs3110 (Fall 20)

let swapped = new_schedule "Sch6" "CAS"
let _ = add_sem swapped (create_sem (Fall 20))
let _ = add_sem swapped (create_sem (Spring 21))
let _ = add_course swapped cs2800 (Spring 21)
let _ = add_course swapped cs3110 (Fall 20)
let _ = swap_courses "CS2800" "CS3110" swapped

let unmoved = new_schedule "Sch7" "CAS"
let _ = add_sem unmoved (create_sem (Fall 20))
let _ = add_sem unmoved (create_sem (Spring 21))
let _ = add_course unmoved cs2800 (Spring 21)
let _ = add_course unmoved cs3110 (Fall 20)
let _ = add_course unmoved cs4820 (Spring 21)

let moved = new_schedule "Sch8" "CAS"
let _ = add_sem moved (create_sem (Fall 20))
let _ = add_sem moved (create_sem (Spring 21))
let _ = add_course moved cs2800 (Spring 21)
let _ = add_course moved cs3110 (Fall 20)
let _ = add_course moved cs4820 (Spring 21)
let _ = move_course "CS2800" (Fall 20) moved

let basic_schedule_tests = [
  make_list_test "Semesters are added to schedule successfully"
    ["FA19"; "SP20"] (sem_ids_to_string sch);
  make_list_test "Semester removed from schedule successfully" ["SP20"]
    (sem_ids_to_string sch_rem_sem);
  make_int_test "Courses added to schedule successfully (number)" 
    4 (to_list sch |> List.length);
  make_string_test "Courses added to schedule successfully (print)"
    "CS3110CS2800PHYS2213CS4820" (string_of_sch sch);
  make_int_test "Schedule has correct number of credits" 15
    (get_credits sch);
  make_string_test "Schedule has correct GPA" "2.80"
    (get_gpa sch |> gpa_to_string);
  make_string_test "Schedule correctly removed course (print)"
    "CS4820CS3110" (string_of_sch sch_rem_sem);
  make_int_test "Schedule correctly removed course (number)"
    2 (to_list sch_rem_sem |> List.length);
  make_int_test "Removed course schedule has correct number of credits"
    8 (get_credits sch_rem_sem);
  make_string_test "Removed course schedule has correct GPA"
    "2.65" (get_gpa sch_rem_sem |> gpa_to_string);
  make_int_test "Edited course credits - check num credits" 3 
    (get_credits edit_creds_sch);
  make_string_test "Edited course credits - check GPA" "3.70"
    (get_gpa edit_creds_sch |> gpa_to_string);
  make_string_test "Edited course grade - check GPA" "3.00"
    (get_gpa edit_grade |> gpa_to_string);
  make_string_test "Schedule added two courses successfully" 
    "CS3110CS2800" (string_of_sch unswapped);
  make_string_test "Schedule swapped the two courses in the previous test
  successfully" "CS2800CS3110" (string_of_sch swapped);
  make_string_test "Schedule added three courses successfully"
    "CS3110CS4820CS2800" (string_of_sch unmoved);
  make_string_test "Schedule moved CS2800 successfully from the previous test"
    "CS2800CS3110CS4820" (string_of_sch moved);
  make_add_sem_test "Schedule added a semester that already exists"
    sch fall19 (DuplicateSemester "FA19");
  make_rem_sem_test "Schedule removed a semester that doesn't exist"
    sch (Fall 20) (UnknownSemester "FA20");
  make_add_course_test "Schedule adds a course that already exists"
    sch cs2800 (Fall 19) (DuplicateCourse "CS2800 already in schedule.");
  make_add_course_test "Schedule adds a course to a semester that doesn't 
  exist" sch btry3080 (Fall 20) (UnknownSemester "FA20");
  make_rem_course_test "Schedule removes a course that doesn't exist"
    sch "BTRY3080" (UnknownCourse "BTRY3080");
  make_edit_course_test "Schedule edits a course with an invalid attribute"
    sch "CS2800" "location" "Hollister" (InvalidAttribute "location");
  make_edit_course_test "Schedule edits a course that doesn't exist"
    sch "BTRY3080" "credits" "3" (UnknownCourse "BTRY3080");
  make_swap_courses_test "Schedule swaps courses in the same semester"
    sch "CS2800" "CS3110" (InvalidSwap);
  make_move_course_test "Schedule moves course to the semester its already in"
    sch "CS2800" (Fall 19) (InvalidMove);
]

(*
    LoadJSON Tests
*)

let example_sch = LoadJSON.parse_json "example.json"
let fa19_courses = get_sem example_sch (Fall 19) |> get_sem_courses
let sp20_courses = get_sem example_sch (Spring 20) |> get_sem_courses

let load_schedule_tests = [
  make_int_test "Credits of example.json schedule" 15 (get_credits example_sch);
  make_string_test
    "Cumulative GPA for example.json" "3.03" 
    (gpa (to_list example_sch) |> gpa_to_string);
  make_string_test 
    "Desc for example.json schedule" "Example Schedule" (get_name example_sch);
  make_string_test 
    "FA19 GPA for example.json sch" "3.30" (gpa fa19_courses |> gpa_to_string);
  make_int_test "Correct number of courses in FA19 in example.json" 2
    (List.length fa19_courses);
  make_string_test
    "SP20 GPA for example.json sch" "2.80" (gpa sp20_courses |> gpa_to_string);
  make_int_test "Correct number of courses in SP20 in example.json" 2
    (List.length sp20_courses);
  make_string_test "Sch is correct in example.json" 
    "CS2800CS3110PHYS2213CS4820" (string_of_sch example_sch)
]

(*
    SaveJSON Tests
    These tests work by saving and then re-loading a schedule.
*)

(* Uses basic schedule from Schedule Tests *)
let _ = SaveJSON.save_schedule sch "test_case_save.json"
let test_saved_sch = LoadJSON.parse_json "test_case_save.json"

let saved_schedule_tests = [
  make_int_test "Credits of saved schedule" 15 (get_credits example_sch);
  make_int_test "correct # of courses" 4 (List.length (to_list test_saved_sch));
  make_string_test
    "Cumulative GPA for saved sched" "2.80"
    (get_gpa test_saved_sch |> gpa_to_string);
  make_string_test 
    "Desc for example.json schedule" "Sch1" (get_name test_saved_sch);
  make_string_test "Sch is correct in saved JSON" 
    "CS3110CS2800PHYS2213CS4820" (string_of_sch test_saved_sch)
]

(* 
    iCalParser tests
*)

let (ical_courses,ical_sem) = ICalParser.parse_file "example.ics"

let ical_tests = [
  make_string_test "Sem in example.ics is SP20" "SP20" ical_sem;
  make_int_test 
    "There are 6 courses in example.ics" 6 (List.length ical_courses);
  make_list_test "Courses in iCal file"
    [ "PSYCH1102"; "ECE4450"; "CS4820"; "CS4411"; "CS3300"; "BTRY3080" ]
    ical_courses
]

(* 
    ClassRoster Test Cases
*)

let creds_3110 = ClassRoster.get_course_creds "CS3110" (Fall 19)
let creds_2110 = ClassRoster.get_course_creds "CS2110" (Fall 19)
let not_fws =  ClassRoster.get_FWS_status "CS3110" (Fall 19)
let real_fws = ClassRoster.get_FWS_status "ENGL1105" (Fall 19)
let d_cat = ClassRoster.distribution_category "BIOMI1120" (Fall 19)
let has_d_cat = ClassRoster.get_distribution_status "BIOMI1120" (Fall 19)
let no_d_cat = ClassRoster.get_distribution_status "ENGL1105" (Fall 19)
let b_cat = ClassRoster.breadth_category "AMST1500" (Fall 19)
let has_b_cat = ClassRoster.get_breadth_status "AMST1500" (Fall 19)
let no_b_cat = ClassRoster.get_breadth_status "ENGL1105" (Fall 19)

let class_roster_tests = [
  make_int_test "3110 is for 4 credits" 4 creds_3110;
  make_int_test "2110 is for 3 credits" 3 creds_2110;
  make_bool_test "CS3110 is not an FWS" false not_fws;
  make_bool_test "ENGL1105 is an FWS" true real_fws;
  make_bool_test "BIOMI1120 has a disttrib cat" true has_d_cat;
  make_string_test "BIOMI1120 dist cat" "(BIONLS-AG, OPHLS-AG, PBS-AS)" d_cat;
  make_bool_test "ENGL1105 does not have distrib cat" false no_d_cat;
  make_bool_test "ENGL1105 does not have breadth cat" false no_b_cat;
  make_bool_test "AMST1500 has a breadth cat" true has_b_cat;
  make_string_test "AMST1500 breadth cat" "(GB)" b_cat;
]

(* 
    END Test Cases!
*)

let test_suite = [
  basic_schedule_tests;
  load_schedule_tests;
  saved_schedule_tests;
  ical_tests;
  class_roster_tests
]

let suite = "Main Test Suite" >::: List.flatten test_suite

let _ = run_test_tt_main suite