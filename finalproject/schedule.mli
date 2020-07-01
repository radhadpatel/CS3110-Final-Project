(**
   Data Types and Functions to represent and work with a schedule.
   @author Chris O'Brian (co253), Radha (rdp89), 
   and Lorenzo Scotto di Vettimo (ls769)
*)

(** The type representing the semester, either spring or fall of a year. 
    The [None] value is only for tracking grduation semester for a schedule. *)
type sem_id = Spring of int | Fall of int | None

(** The type representing the grade of a course. *)
type grade = Sat | Unsat | Withdrawn | Incomplete | None | Transfer 
           | Letter of string

(** The type representing the category of a course as per CS degree 
    requirements. *)
type category = PE | FWS | ENGRI | ENGRD | Required | Core | FourThousandPlus | 
                Technical | Specialization| Liberal | AdvisorApproved | 
                MajorApproved | Practicum | Extra | ForeignLanguage | 
                PBS | GB | HB

(** The type representing a course within a schedule. *)
type course

(** The type representing a semester within a schedule. *)
type semester

(** The type representing the available settings for a schedule. *)
type settings

(** The type representing what requirements of the schedule are met and what
    are not met. *)
type validation = {
  sch : string;
  (** Either "ENG" or "CAS" indicating school user is in. *)
  needed: string list;
  (** A list of needed courses *)
  needed_cat: (string * int) list;
  (** A list of categories and how many credits for each is required. *)
  needed_subs : string list list
  (** A list of groups of courses where one of each is requried. *)
}

(** The type representing a whole schedule *)
type schedule

(** [UnknownCourse nm] raised when course with name [nm] passed as a real course
    but isn't recognized as such. *)
exception UnknownCourse of string

(** [UnknownSemester sem] raised when function attempts to work with 
    non-existent semester. *)
exception UnknownSemester of string

(** [UnknownGrade grd] raised when function attempts to work with invalid 
    string representation of a grade. *)
exception UnknownGrade of string

(** [UnknownCategory cat] raised when function attempts to work with invalid
    category of an ENG schedule. *)
exception UnknownCategoryENG of string

(** [UnknownCategory cat] raised when function attempts to work with invalid
    category of a CAS schedule. *)
exception UnknownCategoryCAS of string

(** [UnknownGrade grd] raised when function attempts to work with invalid 
    setting for schedule. *)
exception UnknownSetting of string

(** [UnknownSchool school] raised when function attempts to work with invalid 
    school. *)
exception UnknownSchool of string

(** [DuplicateCourse nm] raised when course with name [nm] is added to a
    semester where a course with same name already exists. *)
exception DuplicateCourse of string

(** [DuplicateSemester sem] raised when semester with string-id [sem] is added 
    to a schedule where a semester with the same id already exists. *)
exception DuplicateSemester of string

(** [InvalidCredits str] raised when a course is to be added to a schedule with  
    an invalid number of credits.  *)
exception InvalidCredits of string

(** [InvalidSwap] is raised when an illegal swap is attempted. Swap is illegal 
    if both courses are in same semester or both are same course. *)
exception InvalidSwap

(** [InvalidMove] is raised when an illegal move is attempted. Move is illegal 
    the new semester is equal to the previous semester where that course was 
    located. *)
exception InvalidMove

(** [InvalidAttribute str] is raised when a course it to be edited with an 
    invalid attribute. *)
exception InvalidAttribute of string

(** [string_of_list strl] is [strl] flattened into a single string of the form
    "[[ el1, el2, ... ]]" where strl = [["el1"; "el2"; ...]]. *)
val string_of_list : string list -> string

(** [gradify str] is the grade represented by [str] where [str] is some grade 
    value represented as a string.
    Requires: [str] is a valid string rep of a grade, like: 
    "A+" or "b" or "unsat" or "w". 
    Raises: [UnknownGrade str] if [str] is not a valid grade 
    representation. *)
val gradify : string -> grade

(** [check_school school] is whether or not school is CAS/ENG. *)
val check_school : string -> bool

(** [categorify str sch] is the category represented by [str] in [sch] where 
    [str] is some category value represented as a string.
    Requires: [str] is a valid string rep of a category, like: 
    "Core" or "Required". 
    Raises: [UnknownCategory str] if [str] is not a valid grade 
    representation. *)
val categorify : string -> category

(** [create_course name cred gr cat] is a new course type with name [name], 
    number of credits [cred], grade [gr], and category [cat]. *)
val create_course : string -> int -> grade -> category -> course

(** [add_course sch c semid] is the schedule with course [c] added to semester
    with id [sem_id].
    Raises: [DuplicateCourse c.name "already in schedule."] if course already 
    exists in the semester. 
    Raises: [UnknownSemester semid] if semid is not in [sch]. *)
val add_course : schedule -> course -> sem_id -> schedule

(** [edit_course sch c attr new_val] is the schedule that results from changing 
    the course field [attr] to [new_val] for course with name [c] in 
    schedule [sch]. 
    Raises: [InvalidAttribute attr] with various error messages if [attr] is not  
    a valid field of course record, or [new_val] is not valid. 
    Raises: [UnknownCourse c.name] if course is not in [sch]. *)
val edit_course : schedule -> string -> string -> string -> schedule

(** [add_course sch c] is the schedule with course name [c] removed. 
    Raises: [UnknownCourse c.name] if [c] is not in 
    [sch].*)
val remove_course : schedule -> string -> schedule

(** [swap_courses c1_name c2_name sch] is [sch] but with [c1_name] and 
    [c2_name] semester's swapped.
    Raises: [InvalidSwap] if [c1_name] and [c2_name] are in same semester. *)
val swap_courses : string -> string -> schedule -> schedule

(** [move_course c_name sem sch] is [sch] but with [c_name] moved to [sem].
    Raises: [InvalidMove] if [sem] is the semester the course is already in. *)
val move_course : string -> sem_id -> schedule -> schedule

(** [get_course name courses] is the course with name [name]
    found in [courses].
    Raises: [UnknownCourse name] if course does not exist in [courses]. *)
val get_course : string -> course list -> course

(** [get_course_name course] is the name of [course] *)
val get_course_name : course -> string

(** [get_course_credits course] is the number of credits of [course]. *)
val get_course_credits : course -> int

(** [get_course_cat course] is the category in string form of [course]. *)
val get_course_cat : course -> schedule -> string

(** [get_sem sch semid] is the semester with the semester id [sem_id]
    if found in [sch]. 
    Raises: [UnknownSemester sem_id] if no such semester exists. *)
val get_sem : schedule -> sem_id -> semester

(** [get_sems sch] is the list of semesters in [sch]. *)
val get_sems : schedule -> semester list

(** [get_sem_courses sem] is a list of all the courses in the semester
    [sem]. *)
val get_sem_courses : semester -> course list

(** [gpa courses] is the GPA of all the courses in [courses] that have been
    given a letter grade. *)
val gpa : course list -> float

(** [gpa_to_string gpa] is the string representation of [gpa]. *)
val gpa_to_string : float -> string

(** [get_gpa sch] is the cumulative GPA of the schedule [sch]. *)
val get_gpa : schedule -> float

(** [get_credits sch] is the sum of all the credits in the schedule [sch]. *)
val get_credits : schedule -> int

(** [get_credits cl] is the calculated sum of credits of each course in [cl] *)
val calc_credits : course list -> int

(** [create_sem semid] is a new empty semester with id [semid]. *)
val create_sem : sem_id -> semester

(** [add_sem sch sem] is [sch] but with semester [sem] added to its
    list of semesters, and GPA updated.
    Raises: [DuplicateSemester semid] if semester with the same id already 
    exists in [sch]. *)
val add_sem : schedule -> semester -> schedule

(** [remove_sem sch sem] is [sch] with the semester [sem] removed from list of 
    semesters. Updates GPA/credits accordingly.
    Raises: [UnkownSemester semid] if semester doesn't exists in the [sch]. *)
val remove_sem : schedule -> sem_id -> schedule

(** [string_of_semid s] is the string representation of semester id [s]. String
    representations are like "FA20" or "SP18", etc. *)
val string_of_semid : sem_id -> string

(** [string_of_grade gr] is the string representation of a grade [gr]. *)
val string_of_grade : grade -> string

(** [string_of_category cat sch] is the string representation of a category 
    [cat] in [sch]. *)
val string_of_category : category -> string

(** [sem_ids sch] is the list of semester ids from each semester in 
    schedule [sch]. *)
val sem_ids : schedule -> sem_id list

(** [sem_ids_to_string sch] is the list of semester ids as strings from each 
    semester in schedule [sch]. *)
val sem_ids_to_string : schedule -> string list

(** [to_list sch] is the list of all courses contained in each semester in 
    [sch]. *)
val to_list : schedule -> course list

(** [new_schedule name] is a new empty schedule with name [name] and school 
    [school] but no courses or semesters. *)
val new_schedule : string -> string -> schedule

(** [print_course sch course] is [()] after printing the components of a course: 
    the name, number of credits, grade, category, and semester. *)
val print_course : schedule -> course -> unit

(** [print_sem sem] is [()] after printing the components of a semester: 
    the semester id, the list of course names, the GPA, and the number of 
    credits. *)
val print_sem : semester -> unit

(** [print_schedule sch] is [()] after printing the components of a schedule: 
    the semesters, the GPA, and the number of credits. *)
val print_schedule : schedule -> unit

(** [get_save_status sch] is whether or not [sch] has been saved. *)
val get_save_status : schedule -> bool

(** [set_save_status sch] is [unit] after setting the save status of 
    [sch]. *)
val set_save_status : schedule -> bool -> unit

(** [autosave sch] is [()] after autosaving [sch] if autosave is asserted.

    Autosave files are saved in working directory with schedule name as 
    file name. *)
val autosave : schedule -> unit

(** [get_name sch] is the user-defined name of schedule [sch]. *)
val get_name : schedule -> string

(** [edit_name sch nm] is [sch] but with name set to [nm]  *)
val edit_name : schedule -> string -> schedule

(** [get_school sch] is either "ENG" or "CAS" depending on school of 
    schedule. *)
val get_school : schedule -> string

(** [set_school sch school] is [()] after setting school value for [sch] to 
    [school] *)
val set_school : schedule -> string -> unit

(** [edit_settings sch attr val] is [sch] with the setting [attr] updated to 
    the new value [val].
    Raises: [UnknownSetting attr] is [attr] is not a valid setting *)
val edit_settings : schedule -> string -> string -> schedule

(** [set_valid sch v_opt] is [()] after setting the valid info of [sch] to 
    v_opt.*)
val set_valid : schedule -> validation option -> unit

module HTML : sig

  (** [export_schedule sch fl] is [()] after taking [sch] and exporting 
      information in visual representation to HTML file given by path [fl]. *)
  val export_schedule : schedule -> string -> unit

end

module LoadJSON : sig 

  (** [parse_json json] is [sch] where [sch] is constructed from information 
      stored in json file [json]. *)
  val parse_json : string -> schedule

end

module SaveJSON : sig

  (** [save_schedule sch fl] is [()] after saving sch as a JSON file into 
      [fl]. *)
  val save_schedule : schedule -> string -> unit

end