decay(K, T, Y) :-
    Y is exp(-K * T).

plot_2d(K) :-
    MaxY = 10,

    forall(
        between(0, MaxY, Row0),
        (
            Row is MaxY - Row0,
            Label is Row / 10,
            format('~1f |', [Label]),

            forall(
                between(0, 15, T),
                (
                    decay(K, T, Y),
                    Pos is round(Y * 10),
                    (Pos =:= Row -> write('*')
                    ; write(' '))
                )
            ),
            nl
        )
    ),

    write('    +---------------'), nl,
    write('     0123456789012345'), nl.

:- initialization(plot_2d(0.3)).