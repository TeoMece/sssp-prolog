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
    new_heap(n),
    graph(G),
    vertex(G, Source),
    vertices(G, Vs),
    delete(Vs, vertex(G, Source), VsNoSource),
    set_infinite_distances(G, VsNoSource),
    assert(distance(G, vertex(G, Source), 0)),
    set_nil_previous(G, VsNoSource),

    neighbors(G, Source, Neighbors),
    set_neighbors_distances(G, Source, Neighbors),
    insert_vs_in_heap(Source, Neighbors, n),

    extract(n, _, Vmin),
    dijkstra_body(Vmin, n).

dijkstra_body(V, n):-
    empty(n),
    !.
dijkstra_body(V, n):-
    extract(n, Weight, Vmin),
    visited(G, Vmin),
    !,
    dijkstra_body(V, n).

dijkstra_body(V, n) :-
    neighbors(G, V, Neighbors),
    set_neighbors_distances(G, V, Neighbors),
    insert_vs_in_heap(V, Neighbors, n),
    extract(n, Weight, Vmin),
    assert(visited(G, V)),
    dijkstra_body(Vmin, n).


set_infinite_distances(G, []).

set_infinite_distances(G, [V | Vs]) :-
    %%retract(distance(G, V, _)),
    assert(distance(G, V, inf)),
    set_infinite_distances(G, Vs).

set_nil_previous(G, []).

set_nil_previous(G, [V | Vs]) :-
    %%retract(previous(G, V, _)),
    assert(previous(G, V, nil)),
    set_nil_previous(G, Vs).


set_neighbors_distances(_, _, []).

set_neighbors_distances(G, Source, [edge(G, Source, V, W) | Neighbors]) :-
    set_neighbors_distances(G, Source, [edge(G, V, Source, W) | Neighbors]).

set_neighbors_distances(G, Source, [edge(G, V, Source, W) | Neighbors]) :-
    distance(G, V, D),
    V < W,
    !,
    change_distance(G, V, W),
    change_previous(G, V, Source),
    set_neighbors_distances(G, Source, Neighbors).

set_neighbors_distances(G, Source, [edge(G, V, Source, W) | Neighbors]) :-
    set_neighbors_distances(G, Source, Neighbors).


insert_vs_in_heap([], _).
insert_vs_in_heap(Source, [edge(G, Source, V, W) | Vs], Heap) :-
    vertex(G, V),
    insert(Heap, W, V),
    insert_vs_in_heap(Vs, Heap).
insert_vs_in_heap(Source, [edge(G, V, Source, W) | Vs], Heap) :-
    vertex(G, V),
    insert(Heap, W, V),
    insert_vs_in_heap(Vs, Heap).
    

