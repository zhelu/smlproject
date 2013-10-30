CM.make "compiler/Parse/parser.cm";

signature PARSEFILE = sig

  (* readFile filename returns the parse tree resulting from reading the source
   * in filename *)
  val readFile : string -> Ast.dec

  (* algebraic datatype to locally describe the types of declarations *)
  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  datatype exptype = VAREXP | FNEXP | FLATAPPEXP | APPEXP | CASEEXP | LETEXP
                   | SEQEXP | INTEXP | WORDEXP | REALEXP | STRINGEXP | CHAREXP
                   | RECORDEXP | LISTEXP | TUPLEEXP | SELECTOREXP
                   | CONSTRAINTEXP | HANDLEEXP | RAISEEXP | IFEXP | ANDALSOEXP
                   | ORELSEEXP | WHILEEXP | MARKEXP | VECTOREXP

  (* get line count of file *)
  val getLineCount : string -> int

  (* get string equivalent of a dec type *)
  val decTypeToString : dectype -> string

  (* get string equivalent of a exp type *)
  val expTypeToString : exptype -> string

  (* all declaration types except for MarkDec and SeqDec *)
  val allDec : dectype list

  (* Maintains a count of items. Based on a comparison function.
   * See function getCountOf *)
  type 'a counter = ('a * 'a -> order) * ('a * int) list

  (* Create an empty counter given a comparison function *)
  val emptyCounter : ('a * 'a -> order) -> 'a counter

  (* increment takes an item of type ''a and a counter on ''a and increases the
   * count for the input object by the specified integer *)
  val increment : 'a -> int -> 'a counter -> 'a counter

  (* Take two counters and merge their counts
   * Algebraic laws:
   * a merge b = b merge a
   * a merge empty = a *)
  val mergeCounters : 'a counter * 'a counter -> 'a counter

  (* countDec (curried) takes a list of declaration types (dectype) and a
   * parse tree and returns a list of declaration type / occurrence pairs. *)
  val countDec : dectype list -> Ast.dec -> dectype counter

  (* findDec (curried) takes a list of declaration types (dectype) and a
   * a parse tree and returns a list of all declaration nodes that match the
   * input list of types. *)
  val findDec : dectype list -> Ast.dec -> Ast.dec list

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

  (* list of expressions we might be interested in when analyzing code.
   * Currently includes FnExp, CaseExp, LetExp, SelectorExp,
   * ConstraintExp, HandleExp, RaiseExp, WhileExp, VectorExp. *)
  val keyExp : exptype list

  (* list of expressions with the exception of MarkExp *)
  val allExp : exptype list

  (* findExp (curried) takes a list of expression types (exptype) and a
   * a parse tree and returns a list of all expression nodes that match the
   * input list of types. *)
  val findExp : exptype list -> Ast.dec -> Ast.exp list

  (* countExp (curried) takes a list of expression types (exptype) and a
   * parse tree and returns a list of expression type / occurrence pairs. *)
  val countExp : exptype list -> Ast.dec -> exptype counter

  (* Given a value and a counter, return the count of the input parameter *)
  val getCountOf : ''a -> ''a counter -> int

  (* expToExpType is essentially a one-to-one mapping between Ast.exp and
   * exptype *)
  val expToExpType : Ast.exp -> exptype

  (* decToDecType is essentially a one-to-one mapping between Ast.dec and
   * dectype *)
  val decToDecType : Ast.dec -> dectype

  (* Given a Ast.FunDec, returns the number of clauses to define the fuction *)
  val getNumberOfClauses : Ast.dec -> int

  (* Given an AST, returns a list of all names of variables in functions *)
  val getAppVars : Ast.dec -> string list

  (* Given a counter of a's, return a list of pairs of a's and their counts *)
  val counterToList : 'a counter -> ('a * int) list

  (* Given a parsetree, find the top level declaration *)
  val getTopLevelDec : Ast.dec -> Ast.dec list

  (* For a given parse tree, look at the declarations by level
   * Specifically, we're looking for fun, val, and struct decs *)
  val countDecLevel : Ast.dec -> (dectype * int) counter

  (* comparator for dectypes *)
  val dectypeCompare : dectype * dectype -> order

  (* comparator for exptypes *)
  val exptypeCompare : exptype * exptype -> order

  (* comparator for dec level counts *)
  val decLevelCompare : (dectype * int) * (dectype * int) -> order

  (* comparator for level paths *)
  val levelPathCompare : dectype list * dectype list -> order

  (* create a counter of level paths showing which declarations are nested
   * inside which other declarations *)
  val countLevelPaths : Ast.dec -> (dectype list) counter
   
  (* filter declaration counter by level *)
  val filterByLevel : (dectype * int) counter -> int -> dectype counter

end

structure ParseFile :> PARSEFILE = struct

  (* see signature *)
  fun readFile filename =
    let val instrm = TextIO.openIn filename
        val src = Source.newSource (filename, instrm, false,
                                     ErrorMsg.defaultConsumer ())
    in SmlFile.parse src before TextIO.closeIn instrm
    end

  (* see signature *)
  fun getLineCount f =
    let fun readAll (inputFile : string) =
          let val ins = TextIO.openIn inputFile
              fun loop i =
                case TextIO.inputLine i
                  of SOME _ => 1 + loop i
                   | NONE => 0
          in loop ins before TextIO.closeIn ins
          end
    in readAll f
    end

  (* foldDecs is a fold-like function for walking a parse tree. The tree-
   * traversal is DFS. *)
  fun foldDecs f acc dec =
    (case dec
       of Ast.MarkDec (m, _) =>
            let val acc' = f (dec, acc)
            in foldDecs f acc' m
            end
        | Ast.OpenDec paths => f (dec, acc)
        | Ast.TypeDec types => f (dec, acc)
        | Ast.OvldDec (symbol, ty, exps) => f (dec, acc)
        | Ast.SeqDec decs =>
            List.foldl (fn (dec, acc') => foldDecs f acc' dec) acc decs
        | Ast.FixDec _ => f (dec, acc)
        | Ast.LocalDec (locals, body) =>
             let val acc' = f (dec, acc)
                 val newAcc = foldDecs f acc' locals
             in foldDecs f newAcc body
             end
        | Ast.FsigDec _ => f (dec, acc)
        | Ast.SigDec _ => f (dec, acc)
        | Ast.FctDec fctdecs =>
                (* walkStrExp extracts declarations from a structure expression
                 * by applying foldDecs on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldDecs f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldDecs f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                (* walkFctExp pulls Ast.dec types out of a functor expression
                 * by processing the sub structure expression *)
                fun walkFctExp f acc (Ast.MarkFct (fctexp, _)) =
                      walkFctExp f acc fctexp
                  | walkFctExp f acc (Ast.BaseFct {body = strexp,...}) =
                      walkStrExp f acc strexp
                  | walkFctExp f acc (Ast.LetFct (letdec, fctexp)) =
                      walkFctExp f (foldDecs f acc letdec) fctexp
                  | walkFctExp f acc _ = acc
                (* walkFctDec pulls Ast.dec types out of a functor declaration
                 * by processing the sub functor expression *)
                fun walkFctDec f acc (Ast.MarkFctb (fctb, _)) =
                      walkFctDec f acc fctb
                  | walkFctDec f acc (Ast.Fctb {def = fctexp, ...}) =
                      walkFctExp f acc fctexp
                val acc' = f (dec, acc)
            in List.foldl (fn (dec, acc) => walkFctDec f acc dec) acc' fctdecs
            end
        | Ast.AbsDec _ => f (dec, acc)
        | Ast.StrDec strdecs =>
                (* walkStrExp extracts declarations from a structure expression
                 * by applying foldDecs on any Ast.dec types it finds *)
            let fun walkStrExp f acc (Ast.BaseStr d) = foldDecs f acc d
                  | walkStrExp f acc (Ast.MarkStr (strexp, _)) =
                      walkStrExp f acc strexp
                  | walkStrExp f acc (Ast.LetStr (letdec, strexp)) =
                      walkStrExp f (foldDecs f acc letdec) strexp
                  | walkStrExp f acc _ = acc
                (* walkStrDec extracts Ast.dec types from a structure
                 * declaration by processing the sub structure expression *)
                fun walkStrDec f acc (Ast.Strb {def = strexp, ...}) =
                      walkStrExp f acc strexp
                  | walkStrDec f acc (Ast.MarkStrb (strb, _)) =
                      walkStrDec f acc strb
                val acc' = f (dec, acc)
            in List.foldl (fn (dec, acc) => walkStrDec f acc dec) acc' strdecs
            end
        | Ast.ExceptionDec _ => f (dec, acc)
        | Ast.AbstypeDec {body = d, ...} => foldDecs f (f (dec, acc)) d
        | Ast.DataReplDec _ => f (dec, acc)
        | Ast.DatatypeDec _ => f (dec, acc)
        | Ast.FunDec (fblist,_) =>
                (* walkExp gets Ast.decs from an expression *)
            let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                      foldDecs f acc d
                  | walkExp f acc (Ast.MarkExp (exp, _)) =
                      walkExp f acc exp
                  | walkExp f acc (Ast.SeqExp exps) =
                      List.foldl (fn (exp, acc) => walkExp f acc exp)
                        acc exps
                  | walkExp f acc (Ast.FlatAppExp items) =
                      List.foldl
                        (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                        acc items
                  | walkExp f acc _ = acc
                (* walkClause gets Ast.decs from a clause by
                 * checking its expression for let bindings *)
                fun walkClause f acc (Ast.Clause {exp = exp, ...}) =
                      walkExp f acc exp
                (* walkFb gets Ast.dec types from a function definition
                   by processing its clauses for letting bindings *)
                fun walkFb f acc (Ast.MarkFb (fb, _)) = walkFb f acc fb
                  | walkFb f acc (Ast.Fb (clauses, _)) =
                      List.foldl (fn (clause, acc) => walkClause f acc clause)
                        acc clauses
                val acc' = f (dec, acc)
            in List.foldl (fn (fb, acc) => walkFb f acc fb) acc' fblist
            end
        | Ast.ValDec (vblist, _) =>
                (* walkExp gets Ast.decs from an expression *)
            let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                      foldDecs f acc d
                  | walkExp f acc (Ast.MarkExp (exp, _)) =
                      walkExp f acc exp
                  | walkExp f acc (Ast.SeqExp exps) =
                      List.foldl (fn (exp, acc) => walkExp f acc exp)
                        acc exps
                  | walkExp f acc (Ast.FlatAppExp items) =
                      List.foldl
                        (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                        acc items
                  | walkExp f acc _ = acc
                (* walkVb gets Ast.decs from a val declaration by checking
                 * its expression for lets *)
                fun walkVb f acc (Ast.MarkVb (vb, _)) = walkVb f acc vb
                  | walkVb f acc (Ast.Vb {exp = e, ...}) = walkExp f acc e
                val acc' = f (dec, acc)
            in List.foldl (fn (vb, acc) => walkVb f acc vb) acc' vblist
            end
        | Ast.ValrecDec (rvblist, _) =>
                (* walkExp gets Ast.decs from an expression *)
            let fun walkExp f acc (Ast.LetExp {dec = d, ...}) =
                      foldDecs f acc d
                  | walkExp f acc (Ast.MarkExp (exp, _)) =
                      walkExp f acc exp
                  | walkExp f acc (Ast.SeqExp exps) =
                      List.foldl (fn (exp, acc) => walkExp f acc exp)
                        acc exps
                  | walkExp f acc (Ast.FlatAppExp items) =
                      List.foldl
                        (fn ({item = exp, ...}, acc) => walkExp f acc exp)
                        acc items
                  | walkExp f acc _ = acc
                (* walkVb gets Ast.decs from a valrec declaration by checking
                 * its expression for lets *)
                fun walkRvb f acc (Ast.MarkRvb (rvb, _)) = walkRvb f acc rvb
                  | walkRvb f acc (Ast.Rvb {exp = e, ...}) = walkExp f acc e
                val acc' = f (dec, acc)
            in List.foldl (fn (rvb, acc) => walkRvb f acc rvb) acc' rvblist
            end)

  (* type check *)
  val _ = op foldDecs : (Ast.dec * 'a -> 'a) -> 'a -> Ast.dec -> 'a

  type 'a counter = ('a * 'a -> order) * ('a * int) list

  (* see signature *)
  fun getCountOf x (_, []) = 0
    | getCountOf x (f, (y, c) :: rest) =
        (case f (x,y)
           of LESS => 0
            | EQUAL => c
            | GREATER => getCountOf x (f, rest))

  (* see signature *)
  fun increment x i (f, []) = (f, [(x, i)])
    | increment x i (f, (y, count) :: rest) =
        (case f (x,y)
           of LESS => (f, (x,i) :: (y,count) :: rest)
            | EQUAL => (f, (y, count + i) :: rest)
            | GREATER =>
                let val (_, r) = increment x i (f, rest)
                in (f, (y, count) :: r)
                end)

  (* see signature *)
  fun emptyCounter f = (f, [])

  datatype dectype = MARKDEC | OPENDEC | TYPEDEC | OVLDDEC | SEQDEC | FIXDEC
                   | LOCALDEC | FSIGDEC | SIGDEC | FCTDEC | ABSDEC | STRDEC
                   | EXCEPTIONDEC | ABSTYPEDEC | DATAREPLDEC | DATATYPEDEC
                   | FUNDEC | VALDEC | VALRECDEC

  datatype exptype = VAREXP | FNEXP | FLATAPPEXP | APPEXP | CASEEXP | LETEXP
                   | SEQEXP | INTEXP | WORDEXP | REALEXP | STRINGEXP | CHAREXP
                   | RECORDEXP | LISTEXP | TUPLEEXP | SELECTOREXP
                   | CONSTRAINTEXP | HANDLEEXP | RAISEEXP | IFEXP | ANDALSOEXP
                   | ORELSEEXP | WHILEEXP | MARKEXP | VECTOREXP

  fun expToExpType (Ast.VarExp _) = VAREXP
    | expToExpType (Ast.FnExp _) = FNEXP
    | expToExpType (Ast.FlatAppExp _) = FLATAPPEXP
    | expToExpType (Ast.AppExp _) = APPEXP
    | expToExpType (Ast.CaseExp _) = CASEEXP
    | expToExpType (Ast.LetExp _) = LETEXP
    | expToExpType (Ast.SeqExp _) = SEQEXP
    | expToExpType (Ast.IntExp _) = INTEXP
    | expToExpType (Ast.WordExp _) = WORDEXP
    | expToExpType (Ast.RealExp _) = REALEXP
    | expToExpType (Ast.StringExp _) = STRINGEXP
    | expToExpType (Ast.CharExp _) = CHAREXP
    | expToExpType (Ast.RecordExp _) = RECORDEXP
    | expToExpType (Ast.ListExp _) = LISTEXP
    | expToExpType (Ast.TupleExp _) = TUPLEEXP
    | expToExpType (Ast.SelectorExp _) = SELECTOREXP
    | expToExpType (Ast.ConstraintExp _) = CONSTRAINTEXP
    | expToExpType (Ast.HandleExp _) = HANDLEEXP
    | expToExpType (Ast.RaiseExp _) = RAISEEXP
    | expToExpType (Ast.IfExp _) = IFEXP
    | expToExpType (Ast.AndalsoExp _) = ANDALSOEXP
    | expToExpType (Ast.OrelseExp _) = ORELSEEXP
    | expToExpType (Ast.WhileExp _) = WHILEEXP
    | expToExpType (Ast.MarkExp _) = MARKEXP
    | expToExpType (Ast.VectorExp _) = VECTOREXP

  fun decToDecType (Ast.ValDec _) = VALDEC
    | decToDecType (Ast.ValrecDec _) = VALRECDEC
    | decToDecType (Ast.FunDec _) = FUNDEC
    | decToDecType (Ast.TypeDec _) = TYPEDEC
    | decToDecType (Ast.DatatypeDec _) = DATATYPEDEC
    | decToDecType (Ast.DataReplDec _) = DATAREPLDEC
    | decToDecType (Ast.AbstypeDec _) = ABSTYPEDEC
    | decToDecType (Ast.ExceptionDec _) = EXCEPTIONDEC
    | decToDecType (Ast.StrDec _) = STRDEC
    | decToDecType (Ast.AbsDec _) = ABSDEC
    | decToDecType (Ast.FctDec _) = FCTDEC
    | decToDecType (Ast.SigDec _) = SIGDEC
    | decToDecType (Ast.FsigDec _) = FSIGDEC
    | decToDecType (Ast.LocalDec _) = LOCALDEC
    | decToDecType (Ast.SeqDec _) = SEQDEC
    | decToDecType (Ast.OpenDec _) = OPENDEC
    | decToDecType (Ast.OvldDec _) = OVLDDEC
    | decToDecType (Ast.FixDec _) = FIXDEC
    | decToDecType (Ast.MarkDec _) = MARKDEC

  (* see signature *)
  fun decTypeToString MARKDEC = "MarkDec"
    | decTypeToString OPENDEC = "OpenDec"
    | decTypeToString TYPEDEC = "TypeDec"
    | decTypeToString OVLDDEC = "OvldDec"
    | decTypeToString SEQDEC = "SeqDec"
    | decTypeToString FIXDEC = "FixDec"
    | decTypeToString LOCALDEC = "LocalDeC"
    | decTypeToString FSIGDEC = "FsigDec"
    | decTypeToString SIGDEC = "SigDec"
    | decTypeToString FCTDEC = "FctDec"
    | decTypeToString ABSDEC = "AbsDec"
    | decTypeToString STRDEC = "StrDec"
    | decTypeToString EXCEPTIONDEC = "ExceptionDec"
    | decTypeToString ABSTYPEDEC = "AbstypeDec"
    | decTypeToString DATAREPLDEC = "DatareplDec"
    | decTypeToString DATATYPEDEC = "DatatypeDec"
    | decTypeToString FUNDEC = "FunDec"
    | decTypeToString VALDEC = "ValDec"
    | decTypeToString VALRECDEC = "ValrecDec"

  (* see signature *)
  fun expTypeToString VAREXP = "VarExp"
    | expTypeToString FNEXP = "FnExp"
    | expTypeToString FLATAPPEXP = "FlatAppExp"
    | expTypeToString APPEXP = "AppExp"
    | expTypeToString CASEEXP = "CaseExp"
    | expTypeToString LETEXP = "LetExp"
    | expTypeToString SEQEXP = "SeqExp"
    | expTypeToString INTEXP = "IntExp"
    | expTypeToString WORDEXP = "WordExp"
    | expTypeToString REALEXP = "RealExp"
    | expTypeToString STRINGEXP = "StringExp"
    | expTypeToString CHAREXP = "CharExp"
    | expTypeToString RECORDEXP = "RecordExp"
    | expTypeToString LISTEXP = "ListExp"
    | expTypeToString TUPLEEXP = "TupleExp"
    | expTypeToString SELECTOREXP = "SelectorExp"
    | expTypeToString CONSTRAINTEXP = "ConstraintExp"
    | expTypeToString HANDLEEXP = "HandleExp"
    | expTypeToString RAISEEXP = "RaiseExp"
    | expTypeToString IFEXP = "IfExp"
    | expTypeToString ANDALSOEXP = "AndalsoExp"
    | expTypeToString ORELSEEXP = "OrelseExp"
    | expTypeToString WHILEEXP = "WhileExp"
    | expTypeToString MARKEXP = "MarkExp"
    | expTypeToString VECTOREXP = "VectorExp"

  (* see signature *)
  fun dectypeCompare (a,b) = 
        String.compare (decTypeToString a, decTypeToString b)

  (* see signature *)
  fun exptypeCompare (a,b) =
        String.compare (expTypeToString a, expTypeToString b)

  (* see signature *)
  fun decLevelCompare ((a,x),(b,y)) =
    (case (dectypeCompare (a,b), Int.compare (x,y))
       of (LESS, _) => LESS
        | (GREATER, _) => GREATER
        | (_, c) => c)

  (* see signature *)
  fun countDec countedDecs parseTree =
    foldDecs (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) countedDecs)
                  of NONE => acc
                   | SOME _ => increment (decToDecType dec) 1 acc))
                                 (emptyCounter dectypeCompare) parseTree

  (* see signature *)
  fun findDec targetDecs parseTree =
    foldDecs (fn (dec, acc) =>
               (case (List.find (fn x => x = decToDecType dec) targetDecs)
                  of NONE => acc
                   | SOME _ => dec :: acc)) [] parseTree

  (* see signature *)
  val allDec = [OPENDEC, TYPEDEC, OVLDDEC, FIXDEC, LOCALDEC, FSIGDEC, SIGDEC,
                FCTDEC, ABSDEC, STRDEC, EXCEPTIONDEC, ABSTYPEDEC, DATAREPLDEC,
                DATATYPEDEC, FUNDEC, VALDEC, VALRECDEC]

  (* see signature *)
  fun getFunName (Ast.FunDec (fb::_, _)) =
            (* Given an Ast.FunDec, extract the first clause *)
        let fun findFirstClause (Ast.MarkFb (fb, _)) = findFirstClause fb
              | findFirstClause (Ast.Fb (clause::_, _)) = clause
            (* Given an Ast.Clause, extract the first pattern *)
            fun findPatterns (Ast.Clause {pats = ps,...}) = ps
            (* Pull the function name from the pattern using fixity *)
            fun findName ({fixity = SOME (symbol),
                            item = _, region = _} :: ps) =
                  SOME (Symbol.name symbol)
              | findName ({fixity = NONE,
                            item = Ast.MarkPat (Ast.FlatAppPat ps, _),
                            region = _} :: _) =
                  findName ps
              | findName (_ :: ps) = findName ps
              | findName [] = NONE
            (* If fixity is defined for any pattern, use first symbol *)
            fun getFirstVarSymbol ({fixity = _, region = _,
                                     item = Ast.MarkPat (Ast.VarPat s,_)} :: ps)
                  = Symbol.name (hd s)
              | getFirstVarSymbol (_ :: ps) = getFirstVarSymbol ps
              | getFirstVarSymbol _ =
                  let exception Impossible in raise Impossible end
        in (case (findName o findPatterns o findFirstClause) fb
              of SOME name => name
               | NONE =>
                   (getFirstVarSymbol o findPatterns o findFirstClause) fb)
        end
    | getFunName _ = ""

  (* see signature *)
  fun isRecursiveFun (dec as (Ast.FunDec (fblist, _))) =
        let val name = getFunName dec
               (* Check the list of expressions for a call to this function. *)
        in let fun checkExpForName (Ast.MarkExp (e, _)) = checkExpForName e
                 | checkExpForName (Ast.SeqExp es) =
                     List.foldl (fn (e, acc) => checkExpForName e orelse acc)
                       false es
                 | checkExpForName (Ast.VarExp path) =
                     List.exists (fn (Symbol.SYMBOL (_, n)) => n = name) path
                 | checkExpForName (Ast.FlatAppExp items) =
                     List.foldl (fn ({item = e, fixity = _, region = _}, acc) =>
                                    checkExpForName e orelse acc)
                                    false items
                 | checkExpForName _ = false
               (* In the function application, drill down into the item of
                * the fixity record. *)
               fun checkItemForName {item = e, fixity = _, region = _} =
                     checkExpForName e
               (* for each clause, look at the evaluated expression and
                * look for a function application. *)
               fun walkExp (Ast.LetExp {expr = e,...}) = walkExp e
                 | walkExp (Ast.CaseExp {rules = rules,...}) =
                     List.foldl (fn (Ast.Rule {exp = e,...}, acc) =>
                                  walkExp e orelse acc) false rules
                 | walkExp (Ast.IfExp {thenCase = t, elseCase = e, ...}) =
                     (walkExp t) orelse (walkExp e)
                 | walkExp (Ast.FlatAppExp items) =
                     List.foldl (fn (i, acc) => checkItemForName i orelse acc)
                       false items
                 | walkExp (Ast.MarkExp (e, _)) = walkExp e
                 | walkExp _ = false
               (* check for a call to this function inside all clauses *)
               fun walkClause (Ast.Clause {exp = e,...}) = walkExp e
               (* Take a function declaration, and return true if any of its
                * clauses call this function *)
               fun walkFb (Ast.MarkFb (fb, _)) = walkFb fb
                 | walkFb (Ast.Fb (clauses, _)) =
                     List.foldl (fn (c, acc) => walkClause c orelse acc) false
                       clauses
           in List.foldl (fn (fb, acc) => walkFb fb orelse acc) false fblist
           end
        end
    | isRecursiveFun _ = false

  (* see signature *)
  fun getRecursiveFun parseTree =
        List.filter isRecursiveFun (findDec [FUNDEC] parseTree)

  (* see signature *)
  val keyExp = [FNEXP, CASEEXP, LETEXP, SELECTOREXP, CONSTRAINTEXP,
                HANDLEEXP, RAISEEXP, WHILEEXP, VECTOREXP]

  (* see signature *)
  val allExp = [VAREXP, FNEXP, FLATAPPEXP, APPEXP, CASEEXP, LETEXP, INTEXP,
                WORDEXP, REALEXP, STRINGEXP, CHAREXP, SEQEXP, RECORDEXP,
                LISTEXP, TUPLEEXP, SELECTOREXP, CONSTRAINTEXP, HANDLEEXP,
                RAISEEXP, IFEXP, ANDALSOEXP, ORELSEEXP, WHILEEXP, VECTOREXP]

  (* see signuature *)
  fun foldExprs f acc tree =
    let val decs = findDec allDec tree
        (* drill down into declarations to find top-level expressions in that
         * declaration *)
        fun findExpInDec (Ast.ValDec (vblist, _)) =
              let fun findExpInVb (Ast.Vb {exp = e,...}) = e
                    | findExpInVb (Ast.MarkVb (vb, _)) = findExpInVb vb
              in List.foldl (fn (vb, acc) => findExpInVb vb :: acc) [] vblist
              end
          | findExpInDec (Ast.ValrecDec (rvblist, _)) =
              let fun findExpInRvb (Ast.Rvb {exp = e,...}) = e
                    | findExpInRvb (Ast.MarkRvb (rvb, _)) =
                        findExpInRvb rvb
              in List.foldl (fn (rvb, acc) => findExpInRvb rvb :: acc)
                   [] rvblist
              end
          | findExpInDec (Ast.FunDec (fblist, _)) =
              let fun findExpInFb (Ast.Fb (clauses, _)) =
                    List.foldl (fn (Ast.Clause {exp = e, ...}, acc) => e :: acc)
                      [] clauses
                    | findExpInFb (Ast.MarkFb (fb, _)) = findExpInFb fb
              in List.foldl (fn (fb, acc) => findExpInFb fb @ acc) [] fblist
              end
          | findExpInDec (Ast.OvldDec (_, _, exps)) = exps
          | findExpInDec _ = []
        val topExps = List.concat (map findExpInDec decs)
        (* DFS on expr to find subexpressions *)
        fun walkExprs f acc e =
          let val acc' = f (e, acc)
          in (case e
                of (Ast.FnExp rules) =>
                     List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                  walkExprs f acc exp) acc' rules
                 | (Ast.FlatAppExp expFixitems) =>
                     List.foldl (fn ({item = e, fixity = _, region = _}, acc) =>
                                  walkExprs f acc e)
                       acc' expFixitems
                 | (Ast.AppExp {function = fexp, argument = arg}) =>
                     let val acc'' = walkExprs f acc' fexp
                     in walkExprs f acc'' arg
                     end
                 | (Ast.CaseExp {expr = exp, rules = rules}) =>
                     let val acc'' = walkExprs f acc' exp
                     in List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                     walkExprs f acc exp) acc' rules
                     end
                 | (Ast.LetExp {expr = exp,...}) => walkExprs f acc' exp
                 | (Ast.SeqExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.RecordExp symbolExpPairs) =>
                     List.foldl (fn ((_, e), acc) => walkExprs f acc e) acc'
                       symbolExpPairs
                 | (Ast.ListExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.TupleExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | (Ast.ConstraintExp {expr = exp,...}) =>
                     walkExprs f acc' exp
                 | (Ast.HandleExp {expr = exp, rules = rules}) =>
                     let val acc'' = walkExprs f acc' exp
                     in List.foldl (fn (Ast.Rule {exp = exp,...}, acc) =>
                                  walkExprs f acc exp) acc' rules
                     end
                 | (Ast.RaiseExp exp) => walkExprs f acc' exp
                 | (Ast.IfExp {test = t, thenCase = tt, elseCase = tf}) =>
                     let val acc1 = walkExprs f acc' t
                         val acc2 = walkExprs f acc1 tt
                     in walkExprs f acc2 tf
                     end
                 | (Ast.AndalsoExp (e1, e2)) =>
                     let val acc1 = walkExprs f acc' e1
                     in walkExprs f acc1 e2
                     end
                 | (Ast.OrelseExp (e1, e2)) =>
                     let val acc1 = walkExprs f acc' e1
                     in walkExprs f acc1 e2
                     end
                 | (Ast.WhileExp {test = t, expr = body}) =>
                     let val acc'' = walkExprs f acc' t
                     in walkExprs f acc'' body
                     end
                 | (Ast.MarkExp (exp,_)) => walkExprs f acc' exp
                 | (Ast.VectorExp exps) =>
                     List.foldl (fn (e, acc) => walkExprs f acc e) acc' exps
                 | _ => acc')
          end
    in List.foldl (fn (e, a) => walkExprs f a e) acc topExps
    end

  (* see signature *)
  fun findExp targetExps parseTree =
    foldExprs (fn (exp, acc) =>
                (case (List.find (fn x => x = expToExpType exp) targetExps)
                   of NONE => acc
                    | SOME _ => exp :: acc)) [] parseTree

  (* see signature *)
  fun countExp countedExps parseTree =
    foldExprs (fn (exp, acc) =>
                (case (List.find (fn x => x = expToExpType exp) countedExps)
                   of NONE => acc
                    | SOME _ => increment (expToExpType exp) 1 acc))
              (emptyCounter exptypeCompare) parseTree

  (* see signature *)
  fun getNumberOfClauses (Ast.FunDec ([(Ast.MarkFb (Ast.Fb (cs,_),_))],_)) =
      length cs
  | getNumberOfClauses _ = 0;

  (* see signature *)
  fun getAppVars parseTree =
        (* get all applications *)
    let val apps = map (fn (Ast.FlatAppExp items) => items)
                         (findExp [FLATAPPEXP] parseTree)
        (* items in the fixitem *)
        val items = List.concat apps
        (* Get all VarExps used in function application *)
        val vars = List.filter (fn e => expToExpType e = VAREXP)
                     (map
                       (fn ({item = Ast.MarkExp (e, _),region=_,fixity=_}) => e)
                       items)
        (* Extract the symbol name (unqualified, i.e. structure info is
           discarded) *)
        fun getVarName (Ast.VarExp [s]) n =
              let val sym = Symbol.name s
              in if n = "" then sym else (n ^ "." ^ sym)
              end
          | getVarName (Ast.VarExp (s :: ss)) n =
              let val sym = Symbol.name s
              in if n = "" then getVarName (Ast.VarExp ss) sym
                 else getVarName (Ast.VarExp ss) (n ^ "." ^ sym)
              end
    in map (fn x => getVarName x "") vars
    end


  (* see signature *)
  fun getTopLevelDec (Ast.MarkDec (d, _)) = getTopLevelDec d
    | getTopLevelDec (Ast.SeqDec decs) = List.concat (map getTopLevelDec decs)
    | getTopLevelDec d = [d]

  (* see signature *)
  fun mergeCounters ((_, a), b) =
    List.foldl (fn ((x, i), acc) => increment x i acc) b a


  (* levelTraverseDecs takes functions f & g, a level accumulator, a
   * accumulating counter, and a parse tree. For each declaration
   * in the parse tree, it uses f to compute the item to increment in
   * the counter, and on recursive calls, g to update the level
   * accumulator.
   * f computes the item from a (dec, level accumulator pair) 
   * g computes the new level from a (dec, level accumulator pair) *)
  fun levelTraverseDecs f g level acc dec =
    (case dec
      of Ast.MarkDec (m, _) =>
           levelTraverseDecs f g level acc m
       | Ast.SeqDec decs =>
           List.foldl (fn (d, acc') => levelTraverseDecs f g level acc' d)
             acc decs
       | Ast.LocalDec (locals, body) =>
           let val acc' = increment (f (dec, level)) 1 acc
               val newAcc = levelTraverseDecs f g (g (dec, level)) acc' locals
           in levelTraverseDecs f g (g (dec, level)) newAcc body
           end
       | Ast.FctDec fctdecs =>
               (* walkStrExp extracts declarations from a structure
                * expression by applying levelTraverseDecs on any Ast.dec
                * types it finds *)
           let fun walkStrExp l acc (Ast.BaseStr d) =
                     levelTraverseDecs f g l acc d
                 | walkStrExp l acc (Ast.MarkStr (strexp, _)) =
                     walkStrExp l acc strexp
                 | walkStrExp l acc (Ast.LetStr (letdec, strexp)) =
                     walkStrExp l
                       (levelTraverseDecs f g l acc letdec) strexp
                 | walkStrExp l acc _ = acc
               (* walkFctExp pulls Ast.dec types out of a functor
                * expression by processing sub structure expression *)
               fun walkFctExp l acc (Ast.MarkFct (fctexp, _)) =
                     walkFctExp l acc fctexp
                 | walkFctExp l acc (Ast.BaseFct {body = strexp,...}) =
                     walkStrExp l acc strexp
                 | walkFctExp l acc (Ast.LetFct (letdec, fctexp)) =
                     walkFctExp l
                       (levelTraverseDecs f g l acc letdec) fctexp
                 | walkFctExp l acc _ = acc
               (* walkFctDec pulls Ast.dec types out of a functor
                * declaration by processing the sub functor expression *)
               fun walkFctDec l acc (Ast.MarkFctb (fctb, _)) =
                     walkFctDec l acc fctb
                 | walkFctDec l acc (Ast.Fctb {def = fctexp, ...}) =
                     walkFctExp l acc fctexp
               val acc' = increment (f (dec, level)) 1 acc
           in List.foldl (fn (d, acc) => walkFctDec (g (dec, level)) acc d)
                acc' fctdecs
           end
       | Ast.StrDec strdecs =>
               (* walkStrExp extracts declarations from a structure
                * expression by applying levelTraverseDecs on any Ast.dec
                * types it finds *)
           let fun walkStrExp l acc (Ast.BaseStr d) =
                     levelTraverseDecs f g l acc d
                 | walkStrExp l acc (Ast.MarkStr (strexp, _)) =
                     walkStrExp l acc strexp
                 | walkStrExp l acc (Ast.LetStr (letdec, strexp)) =
                     walkStrExp l (levelTraverseDecs f g l acc letdec) strexp
                 | walkStrExp l acc _ = acc
               (* walkStrDec extracts Ast.dec types from a structure
                * declaration by processing the sub structure
                * expression *)
               fun walkStrDec l acc (Ast.Strb {def = strexp, ...}) =
                     walkStrExp l acc strexp
                 | walkStrDec l acc (Ast.MarkStrb (strb, _)) =
                     walkStrDec l acc strb
               val acc' = increment (f (dec, level)) 1 acc
           in List.foldl (fn (d, acc) => walkStrDec (g (dec, level)) acc d)
                acc' strdecs
           end
       | Ast.AbstypeDec {body = d, ...} =>
           let val acc' = increment (f (dec, level)) 1 acc
           in levelTraverseDecs f g (g (dec, level)) acc' d
           end
       | Ast.FunDec (fblist,_) =>
               (* walkExp gets Ast.decs from an expression *)
           let fun walkExp l acc (Ast.LetExp {dec = d, ...}) =
                     levelTraverseDecs f g l acc d
                 | walkExp l acc (Ast.MarkExp (exp, _)) =
                     walkExp l acc exp
                 | walkExp l acc (Ast.SeqExp exps) =
                     List.foldl (fn (exp, acc) => walkExp l acc exp)
                       acc exps
                 | walkExp l acc (Ast.FlatAppExp items) =
                     List.foldl
                       (fn ({item = exp, ...}, acc) => walkExp l acc exp)
                       acc items
                 | walkExp l acc _ = acc
               (* walkClause gets Ast.decs from a clause by
                * checking its expression for let bindings *)
               fun walkClause l acc (Ast.Clause {exp = exp, ...}) =
                     walkExp l acc exp
               (* walkFb gets Ast.dec types from a function definition
                * by processing its clauses for letting bindings *)
               fun walkFb l acc (Ast.MarkFb (fb, _)) = walkFb l acc fb
                 | walkFb l acc (Ast.Fb (clauses, _)) =
                     List.foldl
                       (fn (clause, acc) => walkClause l acc clause)
                       acc clauses
               val acc' = increment (f (dec, level)) 1 acc
           in List.foldl (fn (fb, acc) => walkFb (g (dec, level)) acc fb) acc'
                fblist
           end
       | Ast.ValDec (vblist, _) =>
               (* walkExp gets Ast.decs from an expression *)
           let fun walkExp l acc (Ast.LetExp {dec = d, ...}) =
                     levelTraverseDecs f g l acc d
                 | walkExp l acc (Ast.MarkExp (exp, _)) =
                     walkExp l acc exp
                 | walkExp l acc (Ast.SeqExp exps) =
                     List.foldl (fn (exp, acc) => walkExp l acc exp)
                       acc exps
                 | walkExp l acc (Ast.FlatAppExp items) =
                     List.foldl
                       (fn ({item = exp, ...}, acc) => walkExp l acc exp)
                       acc items
                 | walkExp l acc _ = acc
               (* walkVb gets Ast.decs from a val declaration by checking
                * its expression for lets *)
               fun walkVb l acc (Ast.MarkVb (vb, _)) = walkVb l acc vb
                 | walkVb l acc (Ast.Vb {exp = e, ...}) = walkExp l acc e
               val acc' = increment (f (dec, level)) 1 acc
           in List.foldl (fn (vb, acc) => walkVb (g (dec, level)) acc vb) acc'
                vblist
           end
       | Ast.ValrecDec (rvblist, _) =>
           let fun walkExp l acc (Ast.LetExp {dec = d, ...}) =
                      levelTraverseDecs f g l acc d
                  | walkExp l acc (Ast.MarkExp (exp, _)) =
                      walkExp l acc exp
                  | walkExp l acc (Ast.SeqExp exps) =
                      List.foldl (fn (exp, acc) => walkExp l acc exp)
                        acc exps
                  | walkExp l acc (Ast.FlatAppExp items) =
                      List.foldl
                        (fn ({item = exp, ...}, acc) =>
                          walkExp l acc exp)
                        acc items
                  | walkExp l acc _ = acc
                (* walkVb gets Ast.decs from a valrec declaration by
                 * checking its expression for lets *)
                fun walkRvb l acc (Ast.MarkRvb (rvb, _)) =
                      walkRvb l acc rvb
                  | walkRvb l acc (Ast.Rvb {exp = e, ...}) =
                      walkExp l acc e
                val acc' = increment (f (dec, level)) 1 acc
            in List.foldl (fn (rvb, acc) => walkRvb (g (dec, level)) acc rvb)
                 acc' rvblist
            end
       | _ => increment (f (dec, level)) 1 acc)

  (* type check *)
  val _ =
    op levelTraverseDecs : (Ast.dec * 'a -> 'b) -> (Ast.dec * 'a -> 'a) ->
                             'a -> 'b counter -> Ast.dec -> 'b counter

  (* see signature *)
  fun countDecLevel parseTree =
    levelTraverseDecs (fn (d, l) => (decToDecType d, l)) (fn (_, l) => l + 1) 0
      (emptyCounter decLevelCompare) parseTree

  (* see signature *)
  fun counterToList (f, cs) = cs
  
  (* see signature *)
  fun levelPathCompare (xs, ys) =
    let val (c1, c2) = (length xs, length ys)
    in if c1 = c2
       then List.collate
              (fn (x,y) =>
                String.compare (decTypeToString x, decTypeToString y))
              (xs, ys)
       else Int.compare (c1, c2)
    end

  (* see signature *)
  fun countLevelPaths parseTree =
    let fun f (d, l) = l @ [decToDecType d]
    in levelTraverseDecs f f [] (emptyCounter levelPathCompare) parseTree
    end

  (* see signature *)
  fun filterByLevel (_, levelCounter) level =
    (dectypeCompare,
      map (fn ((x,_),c) => (x,c))
        (List.filter (fn ((_,i),_) => i = level) levelCounter))

end
