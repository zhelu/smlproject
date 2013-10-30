CM.make "compiler/Basics/basics.cm";

signature VIOLATION = sig
  (* given a filename, create a sourcemap tracking line breaks *)
  val getSourceMap : string -> SourceMap.sourcemap

  (* violations come in two forms.
   * offside violations where a declaration/expression at some line contains
   * a violation of a declaration/expression at another line
   * (or) a violation where we've exceeded the maximum width at a line *)
  datatype violation = OFFSIDE of int * int
                     | WIDTH of int

  (* get a list of width violations *)
  val getWidthViolations : string -> violation list
end

structure Violation :> VIOLATION = struct

  datatype violation = OFFSIDE of int * int
                     | WIDTH of int

  (* see signature *)
  fun getSourceMap filename =
    let val instrm = TextIO.openIn filename
        val sourcemap = SourceMap.newSourceMap filename
        fun loop p =
          (case TextIO.input1 instrm
             of NONE => ()
              | SOME e =>
                  let val _ = if e = #"\n" then SourceMap.newline sourcemap p
                              else ()
                  in loop (p + 1)
                  end)
        val _ = loop 1
    in sourcemap before TextIO.closeIn instrm
    end

  fun getWidthViolations filename =
    let val instrm = TextIO.openIn filename
        fun loop l c a =
          (case TextIO.input1 instrm
             of NONE => a
              | SOME e =>
                  if e = #"\n"
                  then loop (l + 1) 1 (if c > 81 then (WIDTH l :: a) else a)
                  else loop l (c + 1) a)
    in List.rev (loop 1 1 []) before TextIO.closeIn instrm
    end

end
