(* ManyFiles allows us to perform many of the operations provides
 * by ParseFile in aggregrate on a list of filenames. See the
 * function getFileList below. *)
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

  (* Count total number of correctly parsed files *)
  val countFiles : string list -> int

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

  (* Given a structure name and a ist of strings and a list of all files,
   * return a counter for the number of times that variable name appears as is
   * or prepended with the structure name in all files *)
  val countAppVars : string -> string list -> string list ->
                       string Counter.counter

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

  (* Given a list of files, filter the list to have only files that contain 
   * only (or at least one) module *)
  val filterIfTopLevelAllModule : string list -> string list
  val filterIfTopLevelHasModule: string list -> string list

  (* Given a list of files, get the FunDecs inside of lets *)
  val getFunsInsideLet : string list -> Ast.dec list
  val getFunsInsideLocal : string list -> Ast.dec list

  (* Given a list of files, get all vals in one of the following forms:
   *   val () = ...
   *   val _ = ... *)
  val getImperativeVals : string list -> Ast.dec list

  (* Given a list of files, get a count of all line widths *)
  val getLineWidthCounts : string list -> int Counter.counter

  (* Given a list of files and a tab size, return a pair indicating unused tab
   * opportunities vs number of tabs *)
  val getTabViolationsVsOpportunities : string list -> int -> int * int
end

structure ManyFiles :> MANYFILES = struct

  structure L = List
  structure P = ParseFile
  structure C = Counter

  (* given a filename, produce SOME parse tree if the parsing is successeful
     or NONE if the parsing fails *)
  fun parseFile f =
    let
      val p = SOME (P.readFile f) handle _ => NONE
    in
      (case p of
         SOME (Ast.SeqDec []) => NONE (* don't count if no decs *)
       | SOME _ => p
       | NONE => NONE)
    end

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
         (case parseFile f >= (fn p => P.countDec decs p) of
            SOME dc => C.mergeInto (dc, acc)
          | NONE => acc)) C.emptyCounter fileList

  (* see signature *)
  fun getExpCount fileList exps =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= (fn p => P.countExp exps p) of
            SOME ec => C.mergeInto (ec, acc)
          | NONE => acc)) C.emptyCounter fileList

  (* see signature *)
  val countTopLevelDecs =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= P.getTopLevelDec >= 
                 (fn dc => map AstType.decToDecType dc) >=
                 (fn ds => L.foldl (fn (d, a) => C.increment d 1 a)
                               C.emptyCounter ds) of
            SOME dc => C.mergeInto (dc, acc)
          | NONE => acc)) C.emptyCounter

  (* see signature *)
  fun countLines fileList =
    let
      val okFiles = L.filter (fn f => parseFile f <> NONE) fileList
      val counts = map P.getLineCount okFiles
    in
      L.foldr (op +) 0 counts
    end

  (* see signature *)
  fun countFiles fileList =
    length (L.filter (fn f => parseFile f <> NONE) fileList)

  (* see signature *)
  val getDecLevelCounts =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= P.countDecLevel of
            SOME dc => C.mergeInto (dc, acc)
          | NONE => acc)) C.emptyCounter

  (* see signature *)
  val getLevelPathCounts =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= P.countLevelPaths of
            SOME dc => C.mergeInto (dc, acc)
          | NONE => acc)) C.emptyCounter

  (* see signature *)
  val filterByLevel = P.filterByLevel

  (* see signature *)
  val findNontrivialSeqExp =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= (fn p => P.findExp [AstType.SEQEXP] p) of
            SOME es =>
              if length
                   (List.filter
                     (fn (Ast.SeqExp xs) => length xs > 1 | _ => false) es) > 1
              then f :: acc
              else acc
          | NONE => acc)) []

  (* see signature *)
  fun countAppVars strct allNames =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f >= ParseFile.getAppVars of
            SOME xs =>
              List.foldl
                (fn (n,a) =>
                  Counter.increment n
                    (length
                      (List.filter
                        (fn x =>
                          x = n orelse String.isSuffix (strct ^ "." ^ n) x) xs))
                    a) acc allNames
          | NONE => acc)) Counter.emptyCounter

  (* see signature *)
  fun getViolations violationF =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f of
            SOME _ => violationF f @ acc
          | NONE => acc)) []

  (* see signature *)
  fun getViolationsByFile violationF =
    L.foldl
      (fn (f, acc) =>
         (case parseFile f of
            SOME _ =>
              let
                val vs = violationF f
              in
                if length vs = 0 then
                  acc
                else (f, violationF f) :: acc
              end
          | NONE => acc)) []

  (* see signature *)
  val filterIfTopLevelAllModule =
    L.filter
      (fn f =>
         (case parseFile f >= ParseFile.isTopLevelOnlyModule of
            SOME t => t
          | NONE => false))

  (* see signature *)
  val filterIfTopLevelHasModule =
    L.filter
      (fn f =>
         (case parseFile f >= ParseFile.topLevelHasModule of
            SOME t => t
          | NONE => false))

  (* see signature *)
  val getFunsInsideLet =
    L.foldl
    (fn (f,a) =>
       (case parseFile f >= (ParseFile.findExp [AstType.LETEXP]) of
          SOME es =>
            a @
            List.filter
              (fn d => AstType.decToDecType d = AstType.FUNDEC)
              (List.concat
                (map
                  (fn (Ast.LetExp {dec = d,...}) =>
                     ParseFile.getTopLevelDec d
                   | _ => let exception Impossible in raise Impossible end) es))
        | NONE => a)) []

  (* see signature *)
  val getFunsInsideLocal =
    L.foldl
    (fn (f,a) =>
       (case parseFile f >= (ParseFile.findDec [AstType.LOCALDEC]) of
          SOME ds =>
            a @
            List.filter
              (fn d => AstType.decToDecType d = AstType.FUNDEC)
              (List.concat
                (map
                  (fn (Ast.LocalDec (d,_)) =>
                     ParseFile.getTopLevelDec d
                   | _ => let exception Impossible in raise Impossible end) ds))
        | NONE => a)) []

  (* see signature *)
  val getImperativeVals =
    L.foldl
      (fn (f,a) =>
         (case parseFile f >= ParseFile.findImperativeVals of
            SOME vs => a @ vs
          | NONE => a)) []
             
  (* see signature *)
  val getLineWidthCounts = 
    L.foldl
      (fn (f,a) =>
         (case parseFile f of
            SOME _ => Counter.mergeInto (Violation.countLineWidth f, a)
          | NONE => a)) Counter.emptyCounter

  (* see signature *)
  fun getTabViolationsVsOpportunities fileList tabsize =
    L.foldl
      (fn (f, a as (opp, tab)) =>
        (case parseFile f of
           SOME _ =>
             let
               val (x, y) = Violation.tabViolationsVsOpportunities f tabsize
             in
               (opp + x, tab + y)
             end
         | NONE => a)) (0,0) fileList
end
