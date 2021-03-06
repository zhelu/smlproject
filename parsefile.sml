(* ParseFile is the main work horse of this project. It performs almost all of
 * the analysis on an ML parse tree. There are handy utility functions for
 * extracting types of declarations or expressions in a single file *)
signature PARSEFILE = sig

  (* readFile filename returns the parse tree resulting from reading the source
   * in filename *)
  val readFile : string -> Ast.dec

  (* get line count of file *)
  val getLineCount : string -> int

  (* countDec (curried) takes a list of declaration types (dectype) and a
   * parse tree and returns a list of declaration type / occurrence pairs. *)
  val countDec : AstType.dectype list -> Ast.dec ->
                   AstType.dectype Counter.counter

  (* findDec (curried) takes a list of declaration types (dectype) and a
   * a parse tree and returns a list of all declaration nodes that match the
   * input list of types. *)
  val findDec : AstType.dectype list -> Ast.dec -> Ast.dec list

  (* Given a declaration of FunDec, return the name of function we're binding *)
  val getFunName : Ast.dec -> string

  (* Given a declaration of FunDec, return true if it's a recursive function.
   * False otherwise. Does not correctly work if you shadow the parent function
   * with a let binding. *)
  val isRecursiveFun : Ast.dec -> bool

  (* Given a parse tree, find all recursive functions *)
  val getRecursiveFun : Ast.dec -> Ast.dec list

  (* foldExprs is a fold-like function on expressions. The order of evaluation
   * is to get all declarations and then examine the declarations for exprs
   * via DFS. *)
  val foldExprs : (Ast.exp * 'a -> 'a) -> 'a -> Ast.dec -> 'a

  (* findExp (curried) takes a list of expression types (exptype) and a
   * a parse tree and returns a list of all expression nodes that match the
   * input list of types. *)
  val findExp : AstType.exptype list -> Ast.dec -> Ast.exp list

  (* countExp (curried) takes a list of expression types (exptype) and a
   * parse tree and returns a list of expression type / occurrence pairs. *)
  val countExp : AstType.exptype list -> Ast.dec ->
                   AstType.exptype Counter.counter

  (* Given a Ast.FunDec, returns the number of clauses to define the fuction *)
  val getNumberOfClauses : Ast.dec -> int

  (* Given an AST, returns a list of all names of variables in functions *)
  val getAppVars : Ast.dec -> string list

  (* Given a parse tree, find the top level declaration *)
  val getTopLevelDec : Ast.dec -> Ast.dec list

  (* Given a parse tree, are the top level decs only signatures, structures,
   * functors, opens, or locals. *)
  val isTopLevelOnlyModule : Ast.dec -> bool

  (* Given a parse tree, does the top level contain a signature, structure,
   * or functor. *)
  val topLevelHasModule : Ast.dec -> bool

  (* For a given parse tree, look at the declarations by level
   * Specifically, we're looking for fun, val, and struct decs *)
  val countDecLevel : Ast.dec -> (AstType.dectype * int) Counter.counter

  (* comparator for level paths *)
  val levelPathCompare : AstType.dectype list * AstType.dectype list -> order

  (* create a counter of level paths showing which declarations are nested
   * inside which other declarations *)
  val countLevelPaths : Ast.dec -> (AstType.dectype list) Counter.counter
   
  (* filter declaration counter by level *)
  val filterByLevel : (AstType.dectype * int) Counter.counter -> int ->
                        AstType.dectype Counter.counter

  (* Given a parse tree, find functions where there is potentially an
   * opportunity for a fold *)
  val findFoldOpportunity : Ast.dec -> Ast.dec list

  (* Given a parse tree, find functions where there is potentially an
   * opportunity for a fold *)
  val findMapFilterOpportunity : Ast.dec -> Ast.dec list

  (* Given a parse tree, find val declarations in one of these forms:
   *   val () = ...
   *   val _ = ... *)
  val findImperativeVals : Ast.dec -> Ast.dec list

end

structure ParseFile :> PARSEFILE = struct

  structure C = Counter
  structure T = AstType
  structure A = Ast
  structure L = List

  (* see signature *)
  fun readFile filename =
    let
      val instrm = TextIO.openIn filename
      val src = Source.newSource
                  (filename, instrm, false, ErrorMsg.defaultConsumer ())
    in
      SmlFile.parse src before TextIO.closeIn instrm
    end

  (* see signature *)
  fun getLineCount filename =
    let
      val ins = TextIO.openIn filename
      fun loop i =
        case TextIO.inputLine i of
          SOME _ => 1 + loop i
        | NONE => 0
    in
      loop ins before TextIO.closeIn ins
    end

  (* foldDecs is a fold-like function for walking a parse tree. The tree-
   * traversal is DFS. *)
  fun foldDecs f acc dec =
    let
      (* walkStrExp extracts declarations from a structure expression
       * by applying foldDecs on any Ast.dec types it finds *)
      fun walkStrExp f acc (A.BaseStr d) = foldDecs f acc d
        | walkStrExp f acc (A.MarkStr (strexp, _)) =
            walkStrExp f acc strexp
        | walkStrExp f acc (A.LetStr (letdec, strexp)) =
            walkStrExp f (foldDecs f acc letdec) strexp
        | walkStrExp _ acc _ = acc
      (* walkFctExp pulls Ast.dec types out of a functor expression
       * by processing the sub structure expression *)
      fun walkFctExp f acc (A.MarkFct (fctexp, _)) =
            walkFctExp f acc fctexp
        | walkFctExp f acc (A.BaseFct {body = strexp,...}) =
            walkStrExp f acc strexp
        | walkFctExp f acc (A.LetFct (letdec, fctexp)) =
            walkFctExp f (foldDecs f acc letdec) fctexp
        | walkFctExp _ acc _ = acc
      (* walkFctDec pulls Ast.dec types out of a functor declaration
       * by processing the sub functor expression *)
      fun walkFctDec f acc (A.MarkFctb (fctb, _)) =
            walkFctDec f acc fctb
        | walkFctDec f acc (A.Fctb {def = fctexp, ...}) =
            walkFctExp f acc fctexp
      (* walkStrDec extracts Ast.dec types from a structure
       * declaration by processing the sub structure expression *)
      fun walkStrDec f acc (A.Strb {def = strexp, ...}) =
            walkStrExp f acc strexp
        | walkStrDec f acc (A.MarkStrb (strb, _)) =
            walkStrDec f acc strb
      (* walkExp gets Ast.decs from an expression *)
      fun walkExp f acc (A.LetExp {dec = d, ...}) =
            foldDecs f acc d
        | walkExp f acc (A.MarkExp (exp, _)) =
            walkExp f acc exp
        | walkExp f acc (A.SeqExp exps) =
            L.foldl (fn (exp, acc) => walkExp f acc exp) acc exps
        | walkExp f acc (A.FlatAppExp items) =
            L.foldl (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                acc items
        | walkExp _ acc _ = acc
      (* walkClause gets Ast.decs from a clause by
       * checking its expression for let bindings *)
      fun walkClause f acc (A.Clause {exp = exp, ...}) = walkExp f acc exp
      (* walkFb gets Ast.dec types from a function definition
       * by processing its clauses for let bindings *)
      fun walkFb f acc (A.MarkFb (fb, _)) = walkFb f acc fb
        | walkFb f acc (A.Fb (clauses, _)) =
            L.foldl (fn (clause, acc) => walkClause f acc clause)
              acc clauses
      (* walkVb gets Ast.decs from a val declaration by checking
       * its expression for lets *)
      fun walkVb f acc (A.MarkVb (vb, _)) = walkVb f acc vb
        | walkVb f acc (A.Vb {exp = e, ...}) = walkExp f acc e
      (* walkVb gets Ast.decs from a valrec declaration by checking
       * its expression for lets *)
      fun walkRvb f acc (A.MarkRvb (rvb, _)) = walkRvb f acc rvb
        | walkRvb f acc (A.Rvb {exp = e, ...}) = walkExp f acc e
      val acc' = f (dec, acc)
    in
     (case dec of
        A.MarkDec (m, _) =>
          foldDecs f acc' m
      | A.SeqDec decs =>
          L.foldl (fn (dec, acc') => foldDecs f acc' dec) acc decs
      | A.FixDec _ => acc'
      | A.LocalDec (locals, body) =>
          let
            val newAcc = foldDecs f acc' locals
          in
            foldDecs f newAcc body
          end
      | A.FctDec fctdecs =>
          L.foldl (fn (dec, acc) => walkFctDec f acc dec) acc' fctdecs
      | A.StrDec strdecs =>
          L.foldl (fn (dec, acc) => walkStrDec f acc dec) acc' strdecs
      | A.AbstypeDec {body = d, ...} => foldDecs f (f (dec, acc)) d
      | A.DataReplDec _ => f (dec, acc)
      | A.DatatypeDec _ => f (dec, acc)
      | A.FunDec (fblist,_) =>
          L.foldl (fn (fb, acc) => walkFb f acc fb) acc' fblist
      | A.ValDec (vblist, _) =>
          L.foldl (fn (vb, acc) => walkVb f acc vb) acc' vblist
      | A.ValrecDec (rvblist, _) =>
          L.foldl (fn (rvb, acc) => walkRvb f acc rvb) acc' rvblist
      | _ => acc')
    end

  (* type check *)
  val _ = op foldDecs : (A.dec * 'a -> 'a) -> 'a -> A.dec -> 'a

  (* see signature *)
  fun countDec countedDecs parseTree =
    foldDecs
      (fn (dec, acc) =>
        case (L.find (fn x => x = T.decToDecType dec) countedDecs) of
          NONE => acc
        | SOME _ => C.increment (T.decToDecType dec) 1 acc)
                      C.emptyCounter parseTree

  (* see signature *)
  fun findDec targetDecs parseTree =
    foldDecs
      (fn (dec, acc) =>
        case (L.find (fn x => x = T.decToDecType dec) targetDecs) of
          NONE => acc
        | SOME _ => dec :: acc) [] parseTree

  (* see signature *)
  fun getFunName (A.FunDec (fb::_, _)) =
    let
      (* Given an Ast.FunDec, extract the first clause *)
      fun findFirstClause (A.MarkFb (fb, _)) = findFirstClause fb
        | findFirstClause (A.Fb (clause::_, _)) = clause
        | findFirstClause _ = let exception Impossible in raise Impossible end
      (* Given an Ast.Clause, extract the first pattern *)
      fun findPatterns (A.Clause {pats = ps,...}) = ps
      (* Pull the function name from the pattern using fixity *)
      fun findName ({fixity = SOME (symbol), item = _, region = _} :: ps) =
            SOME (Symbol.name symbol)
        | findName ({fixity = NONE,
                     item = A.MarkPat (A.FlatAppPat ps, _),
                     region = _} :: _) = findName ps
        | findName (_ :: ps) = findName ps
        | findName [] = NONE
      (* If fixity is defined for any pattern, use first symbol *)
      fun getFirstVarSymbol ({fixity = _,
                              region = _,
                              item = A.MarkPat (A.VarPat s,_)} :: ps) =
            Symbol.name (L.last s)
        | getFirstVarSymbol (_ :: ps) = getFirstVarSymbol ps
        | getFirstVarSymbol _ = let exception Impossible in raise Impossible end
    in
      case (findName o findPatterns o findFirstClause) fb of
        SOME name => name
      | NONE => (getFirstVarSymbol o findPatterns o findFirstClause) fb
    end
    | getFunName _ = ""

  (* see signature *)
  fun isRecursiveFun (dec as (A.FunDec (fblist, _))) =
        let
          val name = getFunName dec
          (* Check the list of expressions for a call to this function. *)
          fun checkExpForName (A.MarkExp (e, _)) = checkExpForName e
            | checkExpForName (A.SeqExp es) =
                L.exists checkExpForName es
            | checkExpForName (A.VarExp path) =
                L.exists (fn (Symbol.SYMBOL (_, n)) => n = name) path
            | checkExpForName (A.FlatAppExp items) =
                L.exists (fn {item = e, fixity = _, region = _} =>
                             checkExpForName e) items
            | checkExpForName _ = false
          (* In the function application, drill down into the item of
           * the fixity record. *)
          fun checkItemForName {item = e, fixity = _, region = _} =
                checkExpForName e
          (* for each clause, look at the evaluated expression and
           * look for a function application. *)
          fun walkExp (A.LetExp {expr = e,...}) = walkExp e
            | walkExp (A.CaseExp {rules = rules,...}) =
                L.exists (fn A.Rule {exp = e,...} => walkExp e) rules
            | walkExp (A.IfExp {thenCase = t, elseCase = e, ...}) =
                walkExp t orelse walkExp e
            | walkExp (A.FlatAppExp items) =
                L.exists checkItemForName items
            | walkExp (A.MarkExp (e, _)) = walkExp e
            | walkExp _ = false
          (* check for a call to this function inside all clauses *)
          fun walkClause (A.Clause {exp = e,...}) = walkExp e
          (* Take a function declaration, and return true if any of its
           * clauses call this function *)
          fun walkFb (A.MarkFb (fb, _)) = walkFb fb
            | walkFb (A.Fb (clauses, _)) =
                L.exists walkClause clauses
        in
          L.exists walkFb fblist
        end
    | isRecursiveFun _ = false

  (* see signature *)
  fun getRecursiveFun parseTree =
        L.filter isRecursiveFun (findDec [T.FUNDEC] parseTree)

  (* see signuature *)
  fun foldExprs f acc tree =
    let
      val decs = findDec T.allDec tree
      (* drill down into declarations to find top-level expressions in that
       * declaration *)
      fun findExpInDec (A.ValDec (vblist, _)) =
            let
              fun findExpInVb (A.Vb {exp = e,...}) = e
                | findExpInVb (A.MarkVb (vb, _)) = findExpInVb vb
            in
              map findExpInVb vblist
            end
        | findExpInDec (A.ValrecDec (rvblist, _)) =
            let
              fun findExpInRvb (A.Rvb {exp = e,...}) = e
                | findExpInRvb (A.MarkRvb (rvb, _)) =
                    findExpInRvb rvb
            in
              map findExpInRvb rvblist
            end
        | findExpInDec (A.FunDec (fblist, _)) =
            let
              fun findExpInFb (A.Fb (clauses, _)) =
                    map (fn A.Clause {exp = e, ...} => e) clauses
                | findExpInFb (A.MarkFb (fb, _)) = findExpInFb fb
            in
              L.foldl (fn (fb, acc) => findExpInFb fb @ acc) [] fblist
            end
        | findExpInDec (A.OvldDec (_, _, exps)) = exps
        | findExpInDec _ = []
      val topExps = L.concat (map findExpInDec decs)
      (* DFS on expr to find subexpressions *)
      fun walkExprs f acc e =
        let val acc' = f (e, acc)
        in
           (case e of
              (A.FnExp rules) =>
                 L.foldl (fn (A.Rule {exp = exp,...}, acc) =>
                              walkExprs f acc exp) acc' rules
            | (A.FlatAppExp expFixitems) =>
                 L.foldl (fn ({item = e, fixity = _, region = _}, acc) =>
                              walkExprs f acc e)
                       acc' expFixitems
            | (A.AppExp {function = fexp, argument = arg}) =>
                let
                  val acc'' = walkExprs f acc' fexp
                in
                  walkExprs f acc'' arg
                end
            | (A.CaseExp {expr = exp, rules = rules}) =>
                let
                  val acc'' = walkExprs f acc' exp
                in
                  L.foldl (fn (A.Rule {exp = exp,...}, acc) =>
                               walkExprs f acc exp) acc' rules
                end
            | (A.LetExp {expr = exp,...}) => walkExprs f acc' exp
            | (A.SeqExp exps) =>
                L.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
            | (A.RecordExp symbolExpPairs) =>
                L.foldl (fn ((_, e), acc) => walkExprs f acc e) acc'
                  symbolExpPairs
            | (A.ListExp exps) =>
                L.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
            | (A.TupleExp exps) =>
                L.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
            | (A.ConstraintExp {expr = exp,...}) =>
                walkExprs f acc' exp
            | (A.HandleExp {expr = exp, rules = rules}) =>
                let
                  val acc'' = walkExprs f acc' exp
                in
                  L.foldl (fn (A.Rule {exp = exp,...}, acc) =>
                               walkExprs f acc exp) acc' rules
                end
            | (A.RaiseExp exp) => walkExprs f acc' exp
            | (A.IfExp {test = t, thenCase = tt, elseCase = tf}) =>
                let
                  val acc1 = walkExprs f acc' t
                  val acc2 = walkExprs f acc1 tt
                in
                  walkExprs f acc2 tf
                end
            | (A.AndalsoExp (e1, e2)) =>
                let
                  val acc1 = walkExprs f acc' e1
                in
                  walkExprs f acc1 e2
                end
            | (A.OrelseExp (e1, e2)) =>
                 let
                   val acc1 = walkExprs f acc' e1
                 in
                   walkExprs f acc1 e2
                 end
            | (A.WhileExp {test = t, expr = body}) =>
                 let
                   val acc'' = walkExprs f acc' t
                 in
                   walkExprs f acc'' body
                 end
            | (A.MarkExp (exp,_)) => walkExprs f acc' exp
            | (A.VectorExp exps) =>
                 L.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
            | _ => acc')
        end
    in
      L.foldl (fn (e, a) => walkExprs f a e) acc topExps
    end

  (* see signature *)
  fun findExp targetExps parseTree =
    foldExprs (fn (exp, acc) =>
                (case (L.find
                        (fn x => x = T.expToExpType exp) targetExps)
                   of NONE => acc
                    | SOME _ => exp :: acc)) [] parseTree

  (* see signature *)
  fun countExp countedExps parseTree =
    foldExprs (fn (exp, acc) =>
                let
                  val exptype = T.expToExpType exp
                in
                  (case (L.find
                          (fn x => x = exptype) countedExps) of
                     NONE => acc
                   | SOME _ =>
                       (case exp of
                          A.SeqExp xs =>
                            if length xs > 1 then
                              C.increment exptype 1 acc
                            else acc
                        | _ => C.increment exptype 1 acc))
                end)
              C.emptyCounter parseTree

  (* see signature *)
  fun getNumberOfClauses (A.FunDec ([(A.MarkFb (A.Fb (cs,_),_))],_)) =
      length cs
  | getNumberOfClauses _ = 0;

  (* see signature *)
  fun getAppVars parseTree =
    let
      (* get all applications *)
      val apps = map (fn (A.FlatAppExp items) => items
                       | _ => let exception Impossible in raise Impossible end)
                       (findExp [T.FLATAPPEXP] parseTree)
      (* items in the fixitem *)
      val items = L.concat apps
      (* Get all VarExps used in function application *)
      val vars = L.filter (fn e => T.expToExpType e = T.VAREXP)
                   (map
                     (fn ({item = A.MarkExp (e, _),region=_,fixity=_}) => e
                       | x => let exception Impossible in raise Impossible end)
                     items)
      (* Extract the symbol name (unqualified, i.e. structure info is
       * discarded) *)
      fun getVarName (A.VarExp [s]) n =
            let
              val sym = Symbol.name s
            in
              if n = "" then sym else (n ^ "." ^ sym)
            end
        | getVarName (A.VarExp (s :: ss)) n =
            let
              val sym = Symbol.name s
            in
              if n = "" then getVarName (A.VarExp ss) sym
              else getVarName (A.VarExp ss) (n ^ "." ^ sym)
            end
        | getVarName _ _ = let exception Bug in raise Bug end
    in
      map (fn x => getVarName x "") vars
    end


  (* see signature *)
  fun getTopLevelDec (A.MarkDec (d, _)) = getTopLevelDec d
    | getTopLevelDec (A.SeqDec decs) = L.concat (map getTopLevelDec decs)
    | getTopLevelDec (A.LocalDec (_, body)) = getTopLevelDec body
    | getTopLevelDec d = [d]

  (* see signature *)
  fun isTopLevelOnlyModule parseTree =
    let
      val decs = getTopLevelDec parseTree
    in
      L.all
        (fn d =>
          (case AstType.decToDecType d of
             AstType.SIGDEC => true
           | AstType.FCTDEC => true
           | AstType.STRDEC => true
           | AstType.OPENDEC => true
           | _ => false)) decs
    end

  (* see signature *)
  fun topLevelHasModule parseTree =
    let
      val decs = getTopLevelDec parseTree
    in
      L.exists
        (fn d =>
          (case AstType.decToDecType d of
             AstType.SIGDEC => true
           | AstType.STRDEC => true
           | AstType.FCTDEC => true
           | _ => false)) decs
    end

  (* levelTraverseDecs takes functions f & g, a level accumulator, a
   * accumulating counter, and a parse tree. For each declaration
   * in the parse tree, it uses f to compute the item to increment
   * the counter, and on recursive calls, g to update the level
   * accumulator.
   * f computes the item from a (dec, level accumulator pair) 
   * g computes the new level from a (dec, level accumulator pair) *)
  fun levelTraverseDecs f g level acc dec =
    let
      (* walkStrExp extracts declarations from a structure
       * expression by applying levelTraverseDecs on any Ast.dec
       * types it finds *)
      fun walkStrExp l acc (A.BaseStr d) = levelTraverseDecs f g l acc d
        | walkStrExp l acc (A.MarkStr (strexp, _)) =
            walkStrExp l acc strexp
        | walkStrExp l acc (A.LetStr (letdec, strexp)) =
            walkStrExp l
              (levelTraverseDecs f g l acc letdec) strexp
        | walkStrExp _ acc _ = acc
      (* walkFctExp pulls Ast.dec types out of a functor
       * expression by processing sub structure expression *)
      fun walkFctExp l acc (A.MarkFct (fctexp, _)) =
            walkFctExp l acc fctexp
        | walkFctExp l acc (A.BaseFct {body = strexp,...}) =
            walkStrExp l acc strexp
        | walkFctExp l acc (A.LetFct (letdec, fctexp)) =
            walkFctExp l (levelTraverseDecs f g l acc letdec) fctexp
        | walkFctExp _ acc _ = acc
      (* walkFctDec pulls Ast.dec types out of a functor
       * declaration by processing the sub functor expression *)
      fun walkFctDec l acc (A.MarkFctb (fctb, _)) =
            walkFctDec l acc fctb
        | walkFctDec l acc (A.Fctb {def = fctexp, ...}) =
            walkFctExp l acc fctexp
      (* walkStrDec extracts Ast.dec types from a structure
       * declaration by processing the sub structure
       * expression *)
      fun walkStrDec l acc (A.Strb {def = strexp, ...}) =
            walkStrExp l acc strexp
        | walkStrDec l acc (A.MarkStrb (strb, _)) =
            walkStrDec l acc strb
      (* walkExp gets Ast.decs from an expression *)
      fun walkExp l acc (A.LetExp {dec = d, ...}) =
            levelTraverseDecs f g l acc d
        | walkExp l acc (A.MarkExp (exp, _)) = walkExp l acc exp
        | walkExp l acc (A.SeqExp exps) =
            L.foldl (fn (exp, acc) => walkExp l acc exp) acc exps
        | walkExp l acc (A.FlatAppExp items) =
            L.foldl
              (fn ({item = exp, ...}, acc) => walkExp l acc exp) acc items
        | walkExp _ acc _ = acc
      (* walkClause gets Ast.decs from a clause by
       * checking its expression for let bindings *)
      fun walkClause l acc (A.Clause {exp = exp, ...}) =
               walkExp l acc exp
      (* walkFb gets Ast.dec types from a function definition
       * by processing its clauses for let bindings *)
      fun walkFb l acc (A.MarkFb (fb, _)) = walkFb l acc fb
        | walkFb l acc (A.Fb (clauses, _)) =
            L.foldl
              (fn (clause, acc) => walkClause l acc clause) acc clauses
      (* walkVb gets Ast.decs from a val declaration by checking
       * its expression for lets *)
      fun walkVb l acc (A.MarkVb (vb, _)) = walkVb l acc vb
        | walkVb l acc (A.Vb {exp = e, ...}) = walkExp l acc e
      (* walkVb gets Ast.decs from a valrec declaration by
       * checking its expression for lets *)
      fun walkRvb l acc (A.MarkRvb (rvb, _)) = walkRvb l acc rvb
        | walkRvb l acc (A.Rvb {exp = e, ...}) =
            walkExp l acc e
      val acc' = C.increment (f (dec, level)) 1 acc
    in
     (case dec of
        A.MarkDec (m, _) =>
          levelTraverseDecs f g level acc m
      | A.SeqDec decs =>
          L.foldl (fn (d, acc') => levelTraverseDecs f g level acc' d)
            acc decs
      | A.LocalDec (locals, body) =>
          let
            val newAcc = levelTraverseDecs f g (g (dec, level)) acc' locals
          in
            levelTraverseDecs f g (g (dec, level)) newAcc body
          end
      | A.FctDec fctdecs =>
          L.foldl (fn (d, acc) => walkFctDec (g (dec, level)) acc d)
            acc' fctdecs
      | Ast.StrDec strdecs =>
          L.foldl (fn (d, acc) => walkStrDec (g (dec, level)) acc d)
            acc' strdecs
      | A.AbstypeDec {body = d, ...} =>
          levelTraverseDecs f g (g (dec, level)) acc' d
      | A.FunDec (fblist,_) =>
          L.foldl (fn (fb, acc) => walkFb (g (dec, level)) acc fb) acc'
            fblist
      | A.ValDec (vblist, _) =>
          L.foldl (fn (vb, acc) => walkVb (g (dec, level)) acc vb) acc'
            vblist
      | Ast.ValrecDec (rvblist, _) =>
          L.foldl (fn (rvb, acc) => walkRvb (g (dec, level)) acc rvb)
            acc' rvblist
      | _ => acc')
    end

  (* type check *)
  val _ =
    op levelTraverseDecs : (A.dec * ''a -> ''b) -> (A.dec * ''a -> ''a) ->
                             ''a -> ''b C.counter -> A.dec -> ''b C.counter

  (* see signature *)
  fun countDecLevel parseTree =
    levelTraverseDecs (fn (d, l) => (T.decToDecType d, l))
      (fn (_, l) => l + 1) 0 C.emptyCounter parseTree

  (* see signature *)
  fun counterToList (f, cs) = cs
  
  (* see signature *)
  fun levelPathCompare (xs, ys) =
    let
      val (c1, c2) = (length xs, length ys)
    in
      if c1 = c2 then
        L.collate
          (fn (x,y) =>
            String.compare (T.decTypeToString x, T.decTypeToString y))
          (xs, ys)
      else Int.compare (c1, c2)
    end

  (* see signature *)
  fun countLevelPaths parseTree =
    let
      fun f (d, l) = l @ [T.decToDecType d]
    in levelTraverseDecs f f [] C.emptyCounter parseTree
    end

  (* see signature *)
  fun filterByLevel levelCounter level =
    C.listToCounter
      (map (fn ((x,_),c) => (x,c))
        (L.filter (fn ((_,i),_) => i = level)
          (C.counterToList levelCounter)))
          
  (* DFS on expr to find subexpressions and apply p to FlatAppExp's *)
  fun searchExpr p (e as A.FlatAppExp expFixitems) =
        L.exists (fn ({item = e, fixity = _, region = _}) => searchExpr p e)
           expFixitems
        orelse p e
    | searchExpr p (A.SeqExp [exp]) = searchExpr p exp
    | searchExpr p (A.AppExp {function = fexp, argument = arg}) =
        searchExpr p fexp orelse searchExpr p arg
    | searchExpr p (A.CaseExp {expr = exp, rules = rules}) =
        searchExpr p exp
        orelse L.exists (fn A.Rule {exp = e,...} => searchExpr p e) rules
    | searchExpr p (A.LetExp {expr = exp,...}) = searchExpr p exp
    | searchExpr p (A.SeqExp exps) =
        L.exists (searchExpr p) exps
    | searchExpr p (A.RecordExp symbolExpPairs) =
        L.exists (fn (_, e) => searchExpr p e) symbolExpPairs
    | searchExpr p (A.ListExp exps) =
        L.exists (searchExpr p) exps
    | searchExpr p (A.TupleExp exps) =
        L.exists (searchExpr p) exps
    | searchExpr p (A.ConstraintExp {expr = exp,...}) = searchExpr p exp
    | searchExpr p (A.HandleExp {expr = exp, rules = rules}) =
        searchExpr p exp
        orelse L.exists (fn A.Rule {exp = e,...} => searchExpr p e) rules
    | searchExpr p (A.RaiseExp exp) = searchExpr p exp
    | searchExpr p (A.IfExp {test = t, thenCase = tt, elseCase = tf}) =
        searchExpr p t
        orelse searchExpr p tt
        orelse searchExpr p tf
    | searchExpr p (A.AndalsoExp (e1, e2)) =
        searchExpr p e1 orelse searchExpr p e2
    | searchExpr p (A.OrelseExp (e1, e2)) =
        searchExpr p e1 orelse searchExpr p e2
    | searchExpr p (A.MarkExp (exp,_)) = searchExpr p exp
    | searchExpr p (A.VectorExp exps) =
        L.exists (searchExpr p) exps
    | searchExpr _ _ = false

  val _ = op searchExpr : (A.exp -> bool) -> A.exp -> bool

  (* see signature *)
  fun findFoldOpportunity parseTree =
    let
      val fs = findDec [AstType.FUNDEC] parseTree
      (* given a Ast.FunDec, find all clauses where we are applying the
       * function to a cons cell. Returns a list of clause * int * string tuple
       * where the int is the position where we find the cons in the clause
       * and the string is the name of the cdr *)
      fun findConsClauses (A.FunDec ([A.MarkFb (A.Fb (cs,_),_)], _)) =
            let
              fun getPatsFromClause (A.Clause {pats = ps,...}) =
                map (fn {item = p, region = _, fixity = _} => p) ps
              fun isPatternCons (A.MarkPat (p,_)) = isPatternCons p
                | isPatternCons
                    (A.FlatAppPat (_ ::
                                   {item = A.MarkPat (A.VarPat cons, _),
                                    fixity = _,
                                    region = _} ::
                                   {item = A.MarkPat (A.VarPat cdr, _),
                                    fixity = _,
                                    region = _} :: _)) =
                    if Symbol.name (L.last cons) = "::" then
                      SOME (Symbol.name (L.last cdr))
                    else NONE
                | isPatternCons _ = NONE
            in
              L.foldl (fn (c,acc) =>
                let
                  val pats = getPatsFromClause c
                  fun findCons _ [] = []
                    | findCons i (p::ps) =
                        (case isPatternCons p of
                           SOME n => (c, i, n) :: findCons (i + 1) ps
                         | NONE => findCons (i + 1) ps)
                in
                  (case findCons 0 pats of
                     [] => acc
                   | xs => acc @ xs)
                end) [] cs
            end
        | findConsClauses _ = []
      (* Given a clause, return true if it uses a cons cell as a pattern and
       * then call itself recursively on the cdr of the cons cell? *)
      fun isRecursiveOnCdr (c as A.Clause {exp = e, pats = pats,...}, i, n) =
        let
          (* Pull the function name from the pattern using fixity *)
          fun findName ({fixity = SOME (symbol), item = _, region = _} :: ps) =
                SOME (Symbol.name symbol)
            | findName ({fixity = NONE,
                         item = A.MarkPat (A.FlatAppPat ps, _),
                         region = _} :: _) = findName ps
            | findName (_ :: ps) = findName ps
            | findName [] = NONE
          (* If fixity is defined for any pattern, use first symbol *)
          fun getFirstVarSymbol ({fixity = _,
                                  region = _,
                                  item = A.MarkPat (A.VarPat s,_)} :: ps) =
                Symbol.name (hd s)
            | getFirstVarSymbol (_ :: ps) = getFirstVarSymbol ps
            | getFirstVarSymbol _ =
                let exception Impossible in raise Impossible end
          val fName = (case findName pats of
                         SOME name => name
                       | NONE => getFirstVarSymbol pats)
          (* Return true if the ith position the FlatAppExp is a VarExp with
           * name n *)
          fun isCdrInPosI i app =
            let
              fun getCdrIndex j
                    (A.FlatAppExp ({item = A.MarkExp (A.VarExp s, _),
                                    ...} :: rest)) =
                      if i = j then
                        Symbol.name (L.last s) = n
                      else getCdrIndex (j + 1) (A.FlatAppExp rest)
                | getCdrIndex _ _ = false
            in
              if i < 1 then
                false
              else getCdrIndex 1 app
            end
          (* return true if the we have a recursive call to the function on
           * the cdr (with name n) in the same position as declared *)
          fun isRecursiveCallOnCdr (A.FlatAppExp ({item = e,...} :: rest)) =
                (case e of
                   A.MarkExp (A.VarExp s,_) =>
                     if Symbol.name (L.last s) = fName then
                       isCdrInPosI i (A.FlatAppExp rest)
                     else isRecursiveCallOnCdr (A.FlatAppExp rest)
                 | _ => isRecursiveCallOnCdr (A.FlatAppExp rest))
            | isRecursiveCallOnCdr _ = false
        in
          if searchExpr isRecursiveCallOnCdr e then SOME i else NONE
        end
      (* check that the ith position is either null or while or some other var *)
      fun isIthWildOrNull i [] = true
        | isIthWildOrNull i (A.Clause {pats = ps,...} :: rest) =
            let
              val pats = map (fn {item = p, fixity = _, region = _} => p) ps
            in
              (case L.nth (pats, i) of
                 A.MarkPat (A.ListPat [], _) => isIthWildOrNull i rest
               | A.MarkPat (A.ListPat _, _) => false
               | _ => isIthWildOrNull i rest)
            end
    in
      L.filter
        (fn f =>
          let
            val cs = findConsClauses f
            val (A.FunDec ((A.MarkFb (A.Fb (allClauses,_),_)) :: _, _)) = f
          in
            (case L.foldl
              (fn (c, acc) =>
                (case isRecursiveOnCdr c of
                   SOME i => SOME i
                 | NONE => acc)) NONE cs of
              SOME i => isIthWildOrNull i allClauses
            | NONE => false)
          end)
      fs
    end

  (* see signature *)
  fun findMapFilterOpportunity parseTree =
    let
      val folds = findFoldOpportunity parseTree
      (* extract clauses from Fb *)
      fun findClauses (A.MarkFb (f, _)) = findClauses f
        | findClauses (A.Fb (cs, _)) = cs
      (* Does the function call itself recursively in the tail of a cons cell *)
      fun hasConsBeforeFName (f as A.FunDec ([fb], _)) =
        let
          val fName = getFunName f
          (* return true if the we have a cons cell with the tail being
           * a recursive call to the function *) 
          fun isConsBeforeFName 
                (A.FlatAppExp
                  (xs as ({item = e1,...} :: {item = e2, ...} :: rest))) =
                (case (e1, e2) of
                   (A.MarkExp (A.VarExp s1,_), A.MarkExp (A.VarExp s2,_)) =>
                     if Symbol.name (L.last s2) = fName 
                        andalso Symbol.name (L.last s1) = "::" then
                        true
                     else isConsBeforeFName (A.FlatAppExp (tl xs))
                 | _ => isConsBeforeFName (A.FlatAppExp (tl xs)))
            | isConsBeforeFName _ = false
        in
          L.exists
            (fn (A.Clause {exp = e,...}) => searchExpr isConsBeforeFName e)
            (findClauses fb)
        end
        | hasConsBeforeFName _ = false
      fun hasNullListRightSide (A.FunDec ([fb], _)) =
        let
          val clauses = findClauses fb
          fun hasNullList (A.FlatAppExp [{item = e,...}]) = hasNullList e
            | hasNullList (A.MarkExp (e, _)) = hasNullList e
            | hasNullList (A.SeqExp [e]) = hasNullList e
            | hasNullList (A.ListExp []) = true
            | hasNullList _ = false
        in
          L.exists (fn (A.Clause {exp = e,...}) => hasNullList e) clauses
        end
        | hasNullListRightSide _ = false
      fun canReplaceWithMapFilter (f as A.FunDec ([fb], _)) =
            hasConsBeforeFName f andalso hasNullListRightSide f
        | canReplaceWithMapFilter _ = false
    in
      L.filter canReplaceWithMapFilter folds
    end

  (* see signature *)
  fun findImperativeVals parseTree =
    let
      val vs = findDec [AstType.VALDEC] parseTree
      (* Given a pattern from a Ast.Vb, evaluates to true if pattern is unit
       * Note that even though unit is written (), it is represented by
       * an empty record. *)
      fun isPatUnitOrWild (A.MarkPat (p,_)) = isPatUnitOrWild p
        | isPatUnitOrWild (A.FlatAppPat [{item = p,...}]) = isPatUnitOrWild p
        | isPatUnitOrWild (A.RecordPat {def = d,...}) = null d
        | isPatUnitOrWild A.WildPat = true
        | isPatUnitOrWild _ = false
      (* Given a ValDec, return true it's of the form val () = ... or
       * val _ = ... *)
      fun isValImperative (A.ValDec ([A.MarkVb (A.Vb {pat = p,...},_)],_)) =
            isPatUnitOrWild p
        | isValImperative _ = false
    in
      L.filter isValImperative vs
    end
end
