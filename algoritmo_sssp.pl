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

set_env(G, Source) :-
    vertices(G, Vs),
    set_infinite_distances(G, Vs),
    retract(distance(G, vertex(G, Source), _)),
    assert(distance(G, vertex(G, Source), 0)),
    set_nil_previous(G, Vs).

dijkstra_sssp(G, V) :-
    visited(G, V), !,
    extract(lista, V, _),
    dijkstra_sssp(G, V).
dijkstra_sssp(G, Start) :- 
    new_heap(lista),
    neighbors(G, Start, Neighbors),
    to_vertices(vertex(G, Start), Neighbors, Vertices),
    set_previous(G, vertex(G, Start), Vertices),
    set_distances(G, Vertices),
    insert_vs(Vertices, lista),
    extract(lista, V, _),
    assert(visited(G, V)),
    dijkstra_sssp(G, V).

set_distances(G, []).
set_distances(G, [V | Vs]) :-
    calculate_distance(G, V),
    set_distances(G, Vs).
    
set_previous(G, _, []).
set_previous(G, S, [vertex(G, V) | Vs]) :-
    change_previous(G, vertex(G, V), S),
    set_previous(G, S, Vs).

calculate_distance(G, V) :-
    previous(G, T, U),
    distance(G, U, PrDist),
    get_edge_weight(G, U, V, Weight),
    NewW is PrDist + Weight,
    change_distance(G, V, NewW).

get_edge_weight(G, U, V, Weight) :-
    edge(G, U, V, Weight), !.
get_edge_weight(G, U, V, Weight) :-
    edge(G, V, U, Weight), !.

to_vertices(_, [], []).
to_vertices(Source, [edge(G, vertex(G, A), Source, _)|Vs], [vertex(G, A)|Vts]) :-
    to_vertices(Source, Vs, Vts).
to_vertices(Source, [edge(G, Source, vertex(G, B), _)|Vs], [vertex(G, B)|Vts]) :-
    to_vertices(Source, Vs, Vts).
   
insert_vs([], _).
insert_vs([vertex(G, V) | Vs], H) :-
    distance(G, vertex(G, V), D),
    insert(H, D, V),
    insert_vs(Vs, H).

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