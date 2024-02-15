:- dynamic heap/2.
:- dynamic heap_entry/4.

%% new_heap/1 - Crea un nuovo heap
new_heap(H) :- 
    heap(H, _), 
    !.
new_heap(H) :- 
    assert(heap(H, 0)), 
    !.

%% heap_size/2 - Restituisce la dimensione dell'heap
heap_size(H, S) :- 
    heap(H, S).
%% delete_heap/1 - Elimina un heap
delete_heap(H) :-
    heap(H, _),
    !,
    retractall(heap(H, _)),
    retractall(heap_entry(H, _, _, _)).

%% empty/1 - Verifica se l'heap è vuoto
empty(H) :-
    heap(H, 0).
%% not_empty/1 - Verifica se l'heap non è vuoto
not_empty(H) :-
    heap(H, S),
    S > 0.
%% head/3 - Restituisce la testa dell'heap
head(H, K, V) :-
    heap_entry(H, 1, K, V).
%% insert/3 - Inserisce un elemento nell'heap
insert(H, K, V) :-
    is_key_not_used(H, K),
    heap_size(H, S),
    NewS is S + 1,
    assert(heap_entry(H, NewS, K, V)),
    retract(heap(H, S)),
    assert(heap(H, NewS)),
    sort_heap(H, 1, NewS).
%% extract/3 - Estrae un elemento dall'heap
extract(H, K, V) :-
    head(H, K, V),
    retract(heap_entry(H, K, V)),
    decrease_position(H),
    heap_size(H, S),
    sort_heap(H, 1, S).
%% decrease_position/1 - Decrementa la posizione di tutti gli elementi dell'heap
decrease_position(H) :-
    heap_size(H, S),
    decrease_position(H, S).

decrease_position(H, 0) :- !.

decrease_position(H, P) :-
    NewP is P - 1,
    decrease_position(H, NewP).
    modify_key(H, P, NewP, _).
%% modify_key/3 - Modifica la chiave di un elemento dell'heap
modify_key(H, NewKey, OldKey, V):-
    is_key_not_used(H, NewKey),
    heap_entry(H, P, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    heap_size(H, S),
    sort_heap(H, 1, S).
%% modify_value/3 - Modifica il valore di un elemento dell'heap
list_heap(H) :-
    listing(heap_entry(H, _, _, _)).
%% modify_value/3 - Modifica il valore di un elemento dell'heap
swap(H, P1, P2) :-
    P1 = P2,
    !.
%% modify_value/3 - Modifica il valore di un elemento dell'heap
swap(H, P1, P2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P1, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)).
%% sort_heap/3 - Ordina l'heap
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
%% min_key/4 - Restituisce la chiave minima dell'heap
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
%% is_key_not_used/2 - Verifica se la chiave è già utilizzata
is_key_not_used(H, K) :-
    \+ heap_entry(H, _, K, _).
