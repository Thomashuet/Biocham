:- initialization(
  catch(spawn_redirect_test, E, (format(user_error, '~p\n', [E]), abort))
).

spawn_redirect_test :-
   spawn_redirect('/bin/sh', ['-c', 'tee'], StreamIn, StreamOut, _StreamErr, _Pid),
   format(StreamIn, '~p.\n', ['foo']),
   read(StreamOut, S),
   S = 'foo'.
