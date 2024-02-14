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
    retract(heap(H, P)),
    NewP is P + 1,
    assert(heap(H, NewP)),
    heapify_down(H, 1).


%%%sort mancante
heapify_up(_H, 1) :- !.

heapify_up(H, P) :-
    heap_entry(H, P, K, V),
    P > 1,
    Par is div(P, 2),
    heap_entry(H, Par, KPar, VPar),
    K < KPar,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, Par, K, V)),
    retract(heap_entry(H, Par, KPar, VPar)),
    assert(heap_entry(H, P, KPar, VPar)),
    heapify_up(H, Par).

heapify_up(_H, _P) :- true.

heapify_down(H, P) :-
    heap_entry(H, P, K, V),
    L is P * 2,
    R is P * 2 + 1,
    heap_entry(H, L, Kl, _Vl),
    heap_entry(H, R, Kr, Vr),
    Kr < Kl,
    Kr < K,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, R, K, V)),
    retract(heap_entry(H, R, Kr, Vr)),
    assert(heap_entry(H, P, Kr, Vr)),
    heapify_down(H, R).

heapify_down(H, P) :-
    heap_entry(H, P, K, V),
    L is P * 2,
    heap_entry(H, L, Kl, Vl),
    Kl < K,
    !,
    retract(heap_entry(H, P, K, V)),
    assert(heap_entry(H, L, K, V)),
    retract(heap_entry(H, L, Kl, Vl)),
    assert(heap_entry(H, P, Kl, Vl)),
    heapify_down(H, L).

heapify_down(_H, _P) :- true.



extract(H, K, V) :-
    head(H, K, V),
    retract(heap_entry(H, K, V)),
    decrease_position(H),
    heapify_down(H, 1).

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
    heapify_down(H, 1).

list_heap(H) :-
    listing(heap_entry(H, _, _, _)).