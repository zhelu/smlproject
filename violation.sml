signature VIOLATION = sig
  (* given a filename, create a sourcemap tracking line breaks *)
  val getSourceMap : string -> SourceMap.sourcemap

  (* violations come in two forms.
   * offside violations where a declaration/expression at some line contains
   * a violation of a declaration/expression at another line
   * (or) a violation where we've exceeded the maximum width at a line *)
  datatype violation = OFFSIDE of { line: int, inside: int }
                     | WIDTH of int
                     | TAB of { line: int, col: int }
                     | IF of int
                     | FOLD of int

  (* get a list of width violations from the input source file *)
  val getWidthViolations : string -> violation list

  (* get a list of tab violations from the input source file *)
  val getTabViolations : string -> violation list

  (* get a list of offside violations from the input source file *)
  val getOffsideViolations : string -> violation list

  (* get a list of if violations from the input source file *)
  val getIfViolations : string -> violation list

  (* get a list of fold violations from the input source file *)
  val getFoldViolations : string -> violation list

  (* given a filename, get a list of all violations *)
  val getViolations : string -> violation list

end

structure Violation :> VIOLATION = struct

  datatype violation = OFFSIDE of { line: int, inside: int }
                     | WIDTH of int
                     | TAB of { line: int, col: int }
                     | IF of int
                     | FOLD of int

  (* see signature *) 
  fun getSourceMap filename =
    let
      val instrm = TextIO.openIn filename
      val sourcemap = SourceMap.newSourceMap filename
      fun loop p =
        case TextIO.input1 instrm of
          NONE => ()
        | SOME e =>
            let
              val _ = if e = #"\n" then
                        SourceMap.newline sourcemap p
                      else ()
              in
                loop (p + 1)
              end
      val _ = loop 1
    in
      sourcemap before TextIO.closeIn instrm
    end

  (* see signature *)
  fun getWidthViolations filename =
    let
      val instrm = TextIO.openIn filename
      fun loop l c a =
        case TextIO.input1 instrm of
          NONE => a
        | SOME e =>
            if e = #"\n" then
              loop (l + 1) 1 (if c > 81 then (WIDTH l :: a) else a)
            else loop l (c + 1) a
    in
      List.rev (loop 1 1 []) before TextIO.closeIn instrm
    end

  (* see signature *)
  fun getTabViolations filename =
    let
      val sourcemap = getSourceMap filename
      val instrm = TextIO.openIn filename
      fun loop p =
        case TextIO.input1 instrm of
          NONE => []
        | SOME e =>
            if e = #"\t" then
              let
                val sourcepos = SourceMap.filepos sourcemap p
              in
                TAB {line = #line(sourcepos),
                     col = #column(sourcepos)} :: loop (p + 1)
              end
            else loop (p + 1)
    in
      loop 1 before TextIO.closeIn instrm
    end

  (* traverseDecs takes a fileloc parameter, a
   * accumulator, a sourcempa, and a parse tree. For each declaration
   * in the parse tree, it checks the MarkDec or MarkExp against
   * the column of the fileloc. If we're at or left of the column,
   * we add a off side violation to the accumulator. When we
   * recurse, we update with the current fileloc.
   * Note: For a MarkX(a,b), we need to use the value a - 1. *)
  fun traverseDecs loc acc sm dec =
    let
      fun addOffside (loc: SourceMap.sourceloc) acc sm p =
        let 
          val {column = c,...} = loc
          val loc' = SourceMap.filepos sm (p - 1)
          val {column = c',...} = loc'
        in
          if c' < c then
            (loc', OFFSIDE {line = #line(loc), inside = #line(loc')} :: acc)
          else (loc', acc)
        end
      (* walkStrExp extracts declarations from a structure
       * expression by applying traverseDecs on any Ast.dec
       * types it finds *)
      fun walkStrExp loc acc sm (Ast.BaseStr d) =
            traverseDecs loc acc sm d
        | walkStrExp loc acc sm (Ast.MarkStr (strexp, _)) =
            walkStrExp loc acc sm strexp
        | walkStrExp loc acc sm (Ast.LetStr (letdec, strexp)) =
            walkStrExp loc (traverseDecs loc acc sm letdec) sm strexp
        | walkStrExp _ acc _ _ = acc
      (* walkFctExp pulls Ast.dec types out of a functor
       * expression by processing sub structure expression *)
      fun walkFctExp loc acc sm (Ast.MarkFct (fctexp, _)) =
            walkFctExp loc acc sm fctexp
        | walkFctExp loc acc sm (Ast.BaseFct {body = strexp,...}) =
            walkStrExp loc acc sm strexp
        | walkFctExp loc acc sm (Ast.LetFct (letdec, fctexp)) =
            walkFctExp loc (traverseDecs loc acc sm letdec) sm fctexp
        | walkFctExp _ acc _ _ = acc
      (* walkFctDec pulls Ast.dec types out of a functor
       * declaration by processing the sub functor expression *)
      fun walkFctDec loc acc sm (Ast.MarkFctb (fctb, _)) =
            walkFctDec loc acc sm fctb
        | walkFctDec loc acc sm (Ast.Fctb {def = fctexp, ...}) =
            walkFctExp loc acc sm fctexp
      (* walkStrDec extracts Ast.dec types from a structure
       * declaration by processing the sub structure
       * expression *)
      fun walkStrDec loc acc sm (Ast.Strb {def = strexp, ...}) =
            walkStrExp loc acc sm strexp
        | walkStrDec loc acc sm (Ast.MarkStrb (strb, _)) =
            walkStrDec loc acc sm strb
      (* walkExp gets Ast.decs from an expression *)
      fun walkExp loc acc sm (Ast.LetExp {dec = d, expr = e}) =
            traverseDecs loc (walkExp loc acc sm e) sm d
        | walkExp loc acc sm (Ast.MarkExp (exp, (p,_))) =
            let
              val (loc', acc') = addOffside loc acc sm p
            in
              walkExp loc' acc' sm exp
            end
        | walkExp loc acc sm (Ast.SeqExp exps) =
            List.foldl (fn (exp, acc) => walkExp loc acc sm exp)
              acc exps
        | walkExp loc acc sm (Ast.FlatAppExp items) =
            List.foldl
              (fn ({item = exp, ...}, acc) => walkExp loc acc sm exp)
              acc items
        | walkExp loc acc sm (Ast.FnExp rules) =
            List.foldl
              (fn (Ast.Rule {exp = e,...}, a) => walkExp loc a sm e) acc rules
        | walkExp loc acc sm (Ast.CaseExp {expr = exp, rules = rules}) =
            List.foldl
              (fn (Ast.Rule {exp = e,...}, a) => walkExp loc a sm e)
              (walkExp loc acc sm exp) rules
        | walkExp loc acc sm (Ast.RecordExp rs) =
            List.foldl
              (fn ((_, e), a) => walkExp loc a sm e) acc rs
        | walkExp loc acc sm (Ast.ListExp es) =
            List.foldl
              (fn (e, a) => walkExp loc a sm e) acc es
        | walkExp loc acc sm (Ast.TupleExp es) =
            List.foldl
              (fn (e, a) => walkExp loc a sm e) acc es
        | walkExp loc acc sm (Ast.HandleExp {expr = exp, rules = rules}) =
            List.foldl
              (fn (Ast.Rule {exp = e,...}, a) => walkExp loc a sm e)
              (walkExp loc acc sm exp) rules
        | walkExp loc acc sm (Ast.VectorExp es) =
            List.foldl
              (fn (e, a) => walkExp loc a sm e) acc es
        | walkExp loc acc sm (Ast.IfExp {test = t,
                                         thenCase = th,
                                         elseCase = el}) =
            let
              val acc' = walkExp loc acc sm t
              val acc'' = walkExp loc acc' sm th
            in
              walkExp loc acc'' sm el
            end
        | walkExp loc acc sm (Ast.AndalsoExp (e1, e2)) =
            let
              val acc' = walkExp loc acc sm e1
            in
              walkExp loc acc' sm e2
            end
        | walkExp loc acc sm (Ast.OrelseExp (e1, e2)) =
            let
              val acc' = walkExp loc acc sm e1
            in
              walkExp loc acc' sm e2
            end
        | walkExp loc acc sm (Ast.WhileExp {test = t, expr = e}) =
            let
              val acc' = walkExp loc acc sm t
            in
              walkExp loc acc' sm e
            end
        | walkExp _ acc _ _ = acc
      (* walkClause gets Ast.decs from a clause by
       * checking its expression for let bindings *)
      fun walkClause loc acc sm (Ast.Clause {exp = exp, ...}) =
            walkExp loc acc sm exp
      (* walkFb gets Ast.dec types from a function definition
       * by processing its clauses for let bindings *)
      fun walkFb loc acc sm (Ast.MarkFb (fb, _)) =
            walkFb loc acc sm fb
        | walkFb loc acc sm (Ast.Fb (clauses, _)) =
            List.foldl
              (fn (clause, acc) => walkClause loc acc sm clause)
              acc clauses
      (* walkVb gets Ast.decs from a val declaration by checking
       * its expression for lets *)
      fun walkVb loc acc sm (Ast.MarkVb (vb, _)) =
            walkVb loc acc sm vb
        | walkVb loc acc sm (Ast.Vb {exp = e, ...}) = walkExp loc acc sm e
      (* walkVb gets Ast.decs from a valrec declaration by
       * checking its expression for lets *)
      fun walkRvb loc acc sm (Ast.MarkRvb (rvb, _)) =
            walkRvb loc acc sm rvb
        | walkRvb loc acc sm (Ast.Rvb {exp = e, ...}) =
             walkExp loc acc sm e
    in
     (case dec of
        Ast.MarkDec (m, (p,_)) =>
          let
            val (loc', acc') = addOffside loc acc sm p
          in
            traverseDecs loc' acc' sm m
          end
      | Ast.SeqDec decs =>
           List.foldl (fn (d, acc') => traverseDecs loc acc' sm d)
             acc decs
      | Ast.LocalDec (locals, body) =>
          let 
            val acc' = traverseDecs loc acc sm locals
          in
            traverseDecs loc acc' sm body
          end
      | Ast.FctDec fctdecs =>
          List.foldl (fn (d, acc) => walkFctDec loc acc sm d)
            acc fctdecs
      | Ast.StrDec strdecs =>
          List.foldl (fn (d, acc) => walkStrDec loc acc sm d)
            acc strdecs
      | Ast.AbstypeDec {body = d, ...} =>
          traverseDecs loc acc sm d
      | Ast.FunDec (fblist,_) =>
          List.foldl (fn (fb, acc) => walkFb loc acc sm fb) acc fblist
      | Ast.ValDec (vblist, _) =>
          List.foldl (fn (vb, acc) => walkVb loc acc sm vb) acc vblist
      | Ast.ValrecDec (rvblist, _) =>
          List.foldl (fn (rvb, acc) => walkRvb loc acc sm rvb)
            acc rvblist
      | _ => acc)
    end

  (* type check *)
  val _ = op traverseDecs : SourceMap.sourceloc -> violation list ->
                            SourceMap.sourcemap -> Ast.dec -> violation list

  (* see signature *)
  fun getOffsideViolations filename =
    let
      val sm = getSourceMap filename
      val parseTree = ParseFile.readFile filename
      val loc = {fileName = filename,
                 line = 0,
                 column = 0}
    in
      traverseDecs loc [] sm parseTree
    end

  (* see signature *)
  fun getIfViolations filename =
    let
      val sm = getSourceMap filename
      val parseTree = ParseFile.readFile filename
      val ifExps = ParseFile.findExp [AstType.IFEXP] parseTree
      (* pull expression for redundant expression *)
      fun getExp (Ast.MarkExp (e, _)) = getExp e
        | getExp (Ast.SeqExp [e]) = getExp e
        | getExp (Ast.FlatAppExp [{item = e, ...}]) = getExp e
        | getExp e = e
      (* get the name of the symbol *)
      fun getSymbol s = Symbol.name (List.last s)
      (* Does the input path list correspond to literal true or false? *)
      fun symbolIsBool s = getSymbol s = "true" orelse getSymbol s = "false"
      (* find a MarkExp so we can get a sourceloc out *)
      fun findMarkPos (Ast.MarkExp (_, (p, _))) = p - 1
        | findMarkPos (Ast.FlatAppExp ({item = e,...} :: _)) = findMarkPos e
        | findMarkPos (Ast.SeqExp [e]) = findMarkPos e
        | findMarkPos _ = let exception Impossible in raise Impossible end
      (* return a optional int for the position in the file if there is an if
       * violation *)
      fun findIfViolation (e as Ast.IfExp {test = test,
                                           thenCase = thenCase,
                                           elseCase = elseCase}) =
            let
              val {fileName = _,
                   line = line,
                   column = _} = SourceMap.filepos sm (findMarkPos thenCase)
            in
              (case (getExp test, getExp thenCase, getExp elseCase) of
                 (Ast.FlatAppExp [{item = Ast.MarkExp (Ast.VarExp testVar, _),...},
                                {item = Ast.MarkExp (Ast.VarExp _, _),...}], _, _) =>
                    if getSymbol testVar = "not" then
                      SOME line
                    else NONE
               | (_, Ast.VarExp thVar, Ast.VarExp elVar) =>
                    if symbolIsBool thVar orelse symbolIsBool elVar then
                      SOME line
                    else NONE
               | (_, Ast.VarExp thVar, _) =>
                    if symbolIsBool thVar then
                        SOME line
                    else NONE
               | (_, _, Ast.VarExp elVar) =>
                    if symbolIsBool elVar then
                      SOME line
                    else NONE
               | _ => NONE)
            end
        | findIfViolation _ = NONE
    in
      List.foldl
        (fn (e, acc) =>
           (case findIfViolation e of
              SOME line => IF line :: acc
            | NONE => acc)) [] ifExps
    end

  (* see signature *)
  fun getFoldViolations filename =
    let
      val parseTree = ParseFile.readFile filename
      val sm = getSourceMap filename
      fun getLine (Ast.FunDec (Ast.MarkFb (_, (p,_)) :: _, _)) =
        let
          val {fileName = _,
               line = line,
               column = _} = SourceMap.filepos sm (p - 1)
        in
          line
        end
      val vs = ParseFile.findFoldOpportunity parseTree
    in
      map (FOLD o getLine) vs
    end
                
  (* see signature *)
  fun getViolations filename =
    let
      val width = getWidthViolations filename
      val tab = getTabViolations filename
      val ifs = getIfViolations filename
      val offside = getOffsideViolations filename
      val folds = getFoldViolations filename
    in
      width @ tab @ ifs @ offside @ folds
    end

end
