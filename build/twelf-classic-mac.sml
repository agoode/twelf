(*
 *
 * This file makes twelf into a library that can be called from
 * a classic Mac OS application (pre Mac OS X).
 *
 * prepare : int -> CharArray.array
 *   expects to be called with the length of the twelf source string
 *   we want to evaluate, so that the sml side can allocate a buffer.
 *   The caller is then expected to write the twelf source into the
 *   buffer returned.
 *
 * execute_for_milliseconds : (int -> int) -> int
 *   once prepare has been called, this will run Twelf for no longer
 *   than the given number of milliseconds. If execution is incomplete,
 *   it will return ~1, and another call can be made to continue execution.
 *   When execution is complete, it will return the int value of
 *   Twelf.Status. Output will be printed to stdout.
 *)

val twelfThread: MLton.Thread.Runnable.t option ref = ref NONE
val mainThread: MLton.Thread.Runnable.t option ref = ref NONE
val status: Twelf.Status option ref = ref NONE
val inputRef: CharArray.array option ref = ref NONE

fun cleanup () = MLton.Thread.atomically (
        fn () => (
            twelfThread := NONE;
            mainThread := NONE;
            status := NONE;
            inputRef := NONE;
            MLton.Itimer.set (MLton.Itimer.Real,
                              {value = Time.zeroTime,
                               interval = Time.zeroTime});
            MLton.Signal.setHandler (Posix.Signal.alrm,
                                     MLton.Signal.Handler.default)
        )
    )

fun twelfRunInThread () =
    let
        val input = valOf (!inputRef)
        val s = Twelf.loadString (CharArraySlice.vector (CharArraySlice.slice (input, 0, SOME (CharArray.length (input) - 1))))
        val mt = valOf (!mainThread)
    in
        (* We have to `switch` to exit the thread cleanly. *)
        MLton.Thread.switch (fn _ => (cleanup ();
                                      (* Save status and return the main thread. *)
                                      status := SOME s;
                                      mt))
    end


val e = _export "prepare": (int -> CharArray.array) -> unit;
val _ = e (fn size =>
              let
                  val input = CharArray.tabulate (size + 1, fn _ => Char.chr 0)
              in
                  cleanup ();
                  twelfThread := SOME (MLton.Thread.prepare (MLton.Thread.new (twelfRunInThread), ()));
                  inputRef := SOME input;
                  input
              end
          )

val e = _export "execute_for_milliseconds": (int -> int) -> unit;
val _ = e (fn ms =>
              let
                  fun codeOfStatus Twelf.OK = 0
                    | codeOfStatus Twelf.ABORT = 1
                  fun switch () = MLton.Thread.switch (
                          fn t =>
                             let
                                 val tt = valOf (!twelfThread)
                             in
                                 mainThread := (SOME (MLton.Thread.prepare (t, ())));
                                 MLton.Signal.setHandler (Posix.Signal.alrm,
                                                          MLton.Signal.Handler.handler (fn t => (twelfThread := SOME t;
                                                                                                 case !mainThread of NONE => t
                                                                                                                   | SOME m => m)));
                                 MLton.Itimer.set (MLton.Itimer.Real,
                                                   {value = Time.fromMilliseconds (Int.toLarge ms),
                                                    interval = Time.zeroTime});
                                 tt
                             end
                      )
                  val prepared = case !twelfThread of NONE => (print "Twelf is unprepared"; false)
                                                    | SOME _ => true
              in
                  case prepared of false => codeOfStatus Twelf.ABORT
                                 | true => (switch ();
                                            case !status of NONE => ~1
                                                          | SOME s => codeOfStatus s)
              end
          )
