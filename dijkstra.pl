:- [graph, minheap, test].

:- dynamic distance/3.
:- dynamic previous/3.
:- dynamic visited/2.

change_distance(G, V, NewDist) :-
    retract(distance(G, V, _)),
    assert(distance(G, V, NewDist)).

change_previous(G, V, U) :-
    retract(previous(G, V, _)),
    assert(previous(G, V, U)).

dijkstra_sssp(G, Source):-
    %%new_heap(n),
    graph(G),
    vertex(G, Source),
    vertices(G, Vs),
    delete(Vs, Source, VsNoSource),
    set_infinite_distances(G, VsNoSource),
    set_nil_previous(G, VsNoSource),
    neighbors(G, Source, Neighbors),
    set_neighbors_distances(G, Source, Neighbors).
    %%insert_vs_in_heap(Neighbors, n).

set_infinite_distances(G, []):-
    assert(distance(G, Source, 0)).

set_infinite_distances(G, [V | Vs]) :-
    retract(distance(G, V, _)),
    assert(distance(G, V, inf)),
    set_infinite_distances(G, Vs).

set_nil_previous(G, []).

set_nil_previous(G, [V | Vs]) :-
    retract(previous(G, V, _)),
    assert(previous(G, V, nil)),
    set_nil_previous(G, Vs).


set_neighbors_distances(_, _, []).
set_neighbors_distances(G, Source, [edge(G, A, B, W) | Neighbors]) :-
    vertex(G, Source) = A,
    !,
    change_distance(G, B, W),
    change_previous(G, B, Source),
    set_neighbors_distances(G, Source, Neighbors).

set_neighbors_distances(G, Source, [edge(G, A, B, W) | Neighbors]) :-
    vertex(G, Source) = B,
    !,
    change_distance(G, A, W),
    change_previous(G, A, Source),
    set_neighbors_distances(G, Source, Neighbors).


insert_vs_in_heap([], _).
insert_vs_in_heap([vertex(G, V) | Vs], Heap) :-
    vertex(G, V),
    distance(G, V, D),
    insert(Heap, V, D),
    insert_vs_in_heap(Vs, Heap).
    

