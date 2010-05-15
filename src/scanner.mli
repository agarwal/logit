(** A scanner specifies how to parse an input file name. *)

type t = private Fmt.t
    (** Similar to general format string but with some restrictions:
        - multiple %s specifiers cannot occur consecutively
        - numeric specifiers can only occur in their simple form: %y, %m, etc. *)
    
(** Constructor. Raise [Failure] if given format string is not a valid
    scanner. *)
val make : Fmt.t -> t

(** [parse_string scanner str] parses [str] using [scanner]. Returns the
    values parsed for each format element. Raise [Failure] if [str] is
    not in format specified by [scanner]. *)
val parse_string : t -> string -> Data.t list
