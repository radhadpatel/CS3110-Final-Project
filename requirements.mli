(**
   Data Types and Functions to store and work with degree requirements
   @author Chris O'Brian (co253), Radha (rdp89), 
   and Lorenzo Scotto di Vettimo (ls769)
*)

open Schedule

(** The type representing the requirements of a CS major in either arts or
    engineering. *)
type reqs

(** The requirements for a CS degree in the College of Engineering. *)
val eng_reqs : reqs

(** The requirements for a CS degree in the College of Arts & Sciences. *)
val cas_reqs : reqs

(** [validate sch reqs] is the validation infromation about [sch] after this
    information has been stored in [sch] based on requirements stated in 
    [reqs]. *)
val validate : schedule -> reqs -> validation

(** [print_validation v] is [()] after printing to the terminal the validation
    information about some schedule whos validation info is [v]. *)
val print_validation : validation -> unit
