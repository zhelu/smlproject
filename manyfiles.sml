signature MANYFILES = sig
  (* given a list of line separated file names,
   * product a list of these names *)
  val getFileList : string -> string list

  (* given a list of files and a list of declarations, compile a tally of
   * the selected expression types for correctly parsed files *)
  val getExpCount : string list ->
                      AstType.exptype list ->
                        AstType.exptype Counter.counter

  (* given a list of files and a list of declarations, compile a tally of
   * the selected declaration types for correctly parsed files *)
  val getDecCount : string list ->
                      AstType.dectype list ->
                        AstType.dectype Counter.counter

  (* Given a list of files, get a count of all top level declarations
   * in correctly parsed files *)
  val countTopLevelDecs : string list -> AstType.dectype Counter.counter

  (* Count total lines in correctly parsed files *)
  val countLines : string list -> int

  (* get counts of fun, val, str declarations and the level of nesting *)
  val getDecLevelCounts : string list ->
                            (AstType.dectype * int) Counter.counter

  (* get counts of fun, val, str declarations and the path of nesting *)
  val getLevelPathCounts : string list ->
                            (AstType.dectype list) Counter.counter

  (* Given a counter of decs by level and a level, filter the counter by
   * level *)
  val filterByLevel : (AstType.dectype * int) Counter.counter -> int ->
                        AstType.dectype Counter.counter

  (* Given a list of files, list all files that contain a SeqExp with
   * length greater than 1 *)
  val findNontrivialSeqExp : string list -> string list

  (* Given a filter predicate and a list of files, count all variable names
   * that satisfy the predicate in all parseable files. *)
  val countAppVars : (string -> bool) -> string list -> int

  (* Given a violation aggregator from the violation structure and a list of
   * files, find all violations of that type in the supplied files. *)
  val getViolations : (string -> Violation.violation list) -> string list ->
                        Violation.violation list

  (* Given a violation aggregator from the violation structure and a list of
   * files, find all violations of that type in the supplied files, returning
   * a list of pairs of files and their violations *)
  val getViolationsByFile : (string -> Violation.violation list) ->
                              string list ->
                              (string * Violation.violation list) list

  (* Given a list of files, get all potential opportunities to refactor with
   * fold *)
  val getFoldOpportunities : string list -> Ast.dec list

  (* Given a list of files, get all potential opportunities to refactor with
   * fold grouped by file *)
  val getFoldOpportunitiesByFile : string list -> (string * Ast.dec list) list

end

structure ManyFiles :> MANYFILES = struct

  structure L = List
  structure P = ParseFile
  structure C = Counter

  (* given a filename, produce SOME parse tree if the parsing is successeful
     or NONE if the parsing fails *)
  fun parseFile f = SOME (P.readFile f) handle _ => NONE

  (* type check *)
  val _ = op parseFile : string -> Ast.dec option

  infix >=

  (* >= sequences operations on option types *)
  fun (SOME s) >= f = SOME (f s)
    | NONE >= f = NONE

  (* type check *)
  val _ = op >= : ('a option * ('a -> 'b)) -> 'b option

  (* see signature *)
  fun getFileList listFile =
    let
      val ins = TextIO.openIn listFile
      fun loop i =
        case TextIO.inputLine i of
          SOME line => line :: loop i
        | NONE => []
    in
      map (fn s => String.extract (s, 0, SOME (size s - 1)))
        (loop ins) before TextIO.closeIn ins
    end

  (* see signature *)
  fun getDecCount fileList decs =
    L.foldl
      (fn (f, acc) =>
        let
          val decCount = parseFile f >= (fn p => P.countDec decs p)
        in
          case decCount of
            SOME dc => C.mergeCounters (dc, acc)
          | NONE => acc
        end) C.emptyCounter fileList

  (* see signature *)
  fun getExpCount fileList exps =
    L.foldl
      (fn (f, acc) =>
        let
          val expCount = parseFile f >= (fn p => P.countExp exps p)
        in
          case expCount of
            SOME ec => C.mergeCounters (ec, acc)
          | NONE => acc
        end) C.emptyCounter fileList

  (* see signature *)
  fun countTopLevelDecs fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val decCount = parseFile f >=
                           P.getTopLevelDec >= 
                           (fn dc => map AstType.decToDecType dc) >=
                           (fn ds => L.foldl
                                        (fn (d, a) => C.increment d 1 a)
                                        C.emptyCounter ds)
        in
          case decCount of
            SOME dc => C.mergeCounters (dc, acc)
          | NONE => acc
        end) C.emptyCounter fileList

  (* see signature *)
  fun countLines fileList =
    let
      val okFiles = L.filter (fn f => parseFile f <> NONE) fileList
      val counts = map P.getLineCount okFiles
    in
      L.foldr (op +) 0 counts
    end

  (* see signature *)
  fun getDecLevelCounts fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val decCount = parseFile f >= P.countDecLevel
        in
          case decCount of
            SOME dc => C.mergeCounters (dc, acc)
          | NONE => acc
        end) C.emptyCounter fileList

  (* see signature *)
  fun getLevelPathCounts fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val decCount = parseFile f >= P.countLevelPaths
        in
          case decCount of
            SOME dc => C.mergeCounters (dc, acc)
          | NONE => acc
        end) C.emptyCounter fileList

  (* see signature *)
  val filterByLevel = P.filterByLevel

  (* see signature *)
  fun findNontrivialSeqExp fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val e = parseFile f >= (fn p => P.findExp [AstType.SEQEXP] p)
        in
          case e of
            SOME es =>
              if length
                   (List.filter (fn (Ast.SeqExp xs) => length xs > 1) es) > 1
              then f :: acc
              else acc
          | NONE => acc
        end) [] fileList

  (* see signature *)
  fun countAppVars p fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val names = parseFile f >= ParseFile.getAppVars
        in
          case names of
            SOME xs => length (List.filter p xs) + acc
          | NONE => acc
        end) 0 fileList

  (* see signature *)
  fun getViolations violationF fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val vs = parseFile f
        in
          case vs of
            SOME _ => violationF f @ acc
          | NONE => acc
        end) [] fileList

  (* see signature *)
  fun getViolationsByFile violationF fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val vs = parseFile f
        in
          case vs of
            SOME _ =>
              let
                val vs = violationF f
              in
                if length vs = 0 then
                  acc
                else (f, violationF f) :: acc
              end
          | NONE => acc
        end) [] fileList

  (* see signature *)
  fun getFoldOpportunities fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val folds = parseFile f >= ParseFile.findFoldOpportunity
        in
          case folds of
            SOME [] => acc
          | SOME os => os @ acc
          | NONE => acc
        end) [] fileList

  (* see signature *)
  fun getFoldOpportunitiesByFile fileList =
    L.foldl
      (fn (f, acc) =>
        let
          val folds = parseFile f >= ParseFile.findFoldOpportunity
        in
          case folds of
            SOME [] => acc
          | SOME os => (f, os) :: acc
          | NONE => acc
        end) [] fileList
end
