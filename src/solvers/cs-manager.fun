(* Constraint Solver Manager *)
(* Author: Roberto Virga *)

functor CSManager (structure Global : GLOBAL
                   structure IntSyn : INTSYN
                   structure Unify : UNIFY
                     sharing Unify.IntSyn = IntSyn
                   structure Fixity : FIXITY
                   structure ModeSyn : MODESYN)
  : CS_MANAGER =
struct
  structure IntSyn  = IntSyn
  structure Fixity  = Fixity
  structure ModeSyn = ModeSyn

  type sigEntry = (* global signature entry *)
    (* constant declaration plus optional precedence and mode information *)
    IntSyn.ConDec * Fixity.fixity option * ModeSyn.ModeSpine option

  type fgnConDec = (* foreign constant declaration *)
    {
      parse : string -> IntSyn.ConDec option
    }

  type solver = (* constraint solver *)
    {
      (* name is the name of the solver *)
      name : string,
      (* names of other constraint solvers needed *)
      needs : string list,
      (* foreign constants declared (if any) *)
      fgnConst : fgnConDec option,
      (* install constants *)
      init : (int * (sigEntry -> IntSyn.cid)) -> unit,
      (* reset internal status *)
      reset : unit -> unit,
      (* trailing operations *)
      mark : unit -> unit,
      unwind : unit -> unit
    }

  exception Error of string

  local

    (* vacuous solver *)
    val emptySolver =
        {
          name = "",
          needs = nil,

          fgnConst = NONE,

          init = (fn _ => ()),

          reset = (fn () => ()),
          mark = (fn () => ()),
          unwind = (fn () => ())
        }

    (* Twelf unification as a constraint solver *)
    val unifySolver =
        {
          name = "Unify",
          needs = nil,

          fgnConst = NONE,

          init = (fn _ => ()),

          reset  = Unify.reset,
          mark   = Unify.mark,
          unwind = Unify.unwind
        }

    (* List of installed solvers *)

    datatype Solver = Solver of solver * bool ref
    
    val maxCS = Global.maxCSid
    val csArray = Array.array (maxCS+1, Solver (emptySolver, ref false)) : Solver Array.array
    val _ = Array.update (csArray, 0, Solver (unifySolver, ref true))
    val nextCS = ref(1)

    (* Installing function *)
    val installFN = ref (fn _ => ~1) : (sigEntry -> IntSyn.cid) ref
    fun setInstallFN f = (installFN := f)

    (* install the specified solver *)
    fun installSolver (solver) =
          let
            val cs = !nextCS
            val _ = if !nextCS > maxCS
                    then raise Error "too many constraint solvers" 
                    else ()
            val _ = Array.update (csArray, cs, Solver (solver, ref false));
            val _ = nextCS := !nextCS+1
          in
            cs
          end

    (* install the unification solver *)
    val _ = installSolver (unifySolver)

    (* make all the solvers inactive *)
    fun resetSolvers () =
          Array.appi (fn (cs, Solver (solver, active)) =>
                            if !active then
                              (
                                active := ((cs = 0) orelse false);
                                #reset(solver) ()
                              )
                            else ())
                     (csArray, 0, SOME(!nextCS))

    (* make the specified solver active *)
    fun useSolver name =
          let
            exception Found of IntSyn.csid
            fun findSolver name =
                  (
                    Array.appi (fn (cs, Solver (solver, _)) =>
                                    if (#name(solver) = name)
                                    then raise Found cs
                                    else ())
                               (csArray, 0, SOME(!nextCS));
                    NONE
                  ) handle Found cs => SOME(cs)
          in
            case findSolver name
              of SOME(cs) =>
                   let
                     val Solver (solver, active) = Array.sub (csArray, cs)
                   in
                     if !active then ()
                     else 
                       (
                          active := true;
                          List.app useSolver (#needs(solver));
                          #init(solver) (cs, !installFN)
                       )
                   end
               | NONE => raise Error ("solver " ^ name ^ " not found")
          end

  (* ask each active solver to try and parse the given string *)
  fun parse string =
        let
          val _ = print ("Searching " ^ string ^ "\n")
          exception Parsed of IntSyn.csid * IntSyn.ConDec
          fun parse' (cs, solver : solver) =
                (case #fgnConst(solver)
                           of NONE => ()
                            | SOME(fgnConDec) =>
                                (case #parse(fgnConDec) (string)
                                   of NONE => ()
                                    | SOME conDec => raise Parsed (cs, conDec)))
        in
          (
            Array.appi (fn (cs, Solver (solver, active)) =>
                          if !active then parse' (cs, solver) else ())
                       (csArray, 0, SOME(!nextCS));
            NONE
          ) handle Parsed info => SOME(info)
        end

  (* reset the internal status of all the active solvers *)
  fun reset () =
        Array.appi (fn (_, Solver (solver, active)) =>
                          if !active then #reset(solver) ()
                          else ())
                   (csArray, 0, SOME(!nextCS));
          

  (* trail the give function *)
  fun trail f =
        let
          val _ = Array.appi (fn (_, Solver (solver, active)) =>
                               if !active then #mark(solver) ()
                               else ())
                             (csArray, 0, SOME(!nextCS))
          val r = f()
          val _ = Array.appi (fn (_, Solver (solver, active)) =>
                               if !active then #unwind(solver) ()
                               else ())
                            (csArray, 0, SOME(!nextCS))
        in
          r
        end
  in
    val setInstallFN = setInstallFN

    val installSolver = installSolver
    val resetSolvers = resetSolvers
    val useSolver = useSolver

    val parse = parse

    val reset = reset
    val trail = trail
  end
end  (* functor CSManager *)