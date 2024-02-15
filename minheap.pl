:- dynamic heap/2.
:- dynamic heap_entry/4.

new_heap(H) :- 
    heap(H, _), 
    !.
new_heap(H) :- 
    assert(heap(H, 0)), 
    !.

heap_size(H, S) :- 
    heap(H, S).

delete_heap(H) :-
    heap(H, _),
    !,
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).
empty(H) :-
    heap(H, 0).

not_empty(H) :-
    heap(H, S),
    S > 0.

head(H, K, V) :-
    heap_entry(H, 1, K, V).

insert(H, K, V) :-
    heap_size(H, S),
    NewS is S + 1,
    assert(heap_entry(H, NewS, K, V)),
    retract(heap(H, S)),
    assert(heap(H, NewS)).
    %%sort_heap(H, 1, S).

extract(H, K, V) :-
    head(H, K, V),
    retract(heap_entry(H, K, V)),
    decrease_position(H),
    heap_size(H, S),
    sort_heap(H, 1, S).

decrease_position(H) :-
    heap_size(H, S),
    decrease_position(H, S).

decrease_position(H, 0) :- !.

decrease_position(H, P) :-
    NewP is P - 1,
    decrease_position(H, NewP).
    modify_key(H, P, NewP, _).

modify_key(H, NewKey, OldKey, V):-
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heap_size(H, S),
    sort_heap(H, 1, S).

list_heap(H) :-
    listing(heap_entry(H, _, _, _)).

swap(H, P1, P2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P1, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)).

sort_heap(H, S, E) :-
    S = E,
    !.

sort_heap(H, S, E) :-
    min_key(H, S, E, Min),
    heap_entry(H, PosMin, Min, _),
    swap(H, S, PosMin),
    NewS is S + 1,
    sort_heap(H, NewS, E),
    !.

min_key(H, S, E, M) :-
    S = E,
    !,
    heap_entry(H, E, M, _).

min_key(H, S, E, M) :-
    S < E,
    !,
    heap_entry(H, S, K, _),
    NewS is S + 1,
    min_key(H, NewS, E, M1),
    M is min(K, M1).
