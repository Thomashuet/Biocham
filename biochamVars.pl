biocham_version('3.5').
%%% msort(L) :- sort0(L).
%%% msort(L1, L2) :- sort0(L1, L2).
%spawn_redirect(_A, _B, _C, _D, _E, _F) :- true.
handle_ctrl_c :-
   '$call_c'('Pl_Set_Ctrl_C_Handler_0'),
   true.
