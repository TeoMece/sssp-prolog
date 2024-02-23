:- [graph, minheap, test].

:- dynamic distance/3.
:- dynamic previous/3.
:- dynamic visited/2.

%% change_distance/3 - cambia la distanza di un vertice
change_distance(G, V, NewDist) :-
    graph(G),
    retract(distance(G, V, _)),
    assert(distance(G, V, NewDist)).

%% change_previous/3 - cambia il vertice precedente di un vertice
change_previous(G, V, U) :-
    graph(G),
    retract(previous(G, V, _)),
    assert(previous(G, V, U)).

%% dijkstra_sssp/2 e dijkstra_ssso_recursive/2 - calcola il cammino minimo da un vertice sorgente a tutti gli altri
dijkstra_sssp(G, vertex(G, Source)) :-
    graph(G),
    retractall(visited(G, _)),
    retractall(distance(G, _, _)),
    retractall(previous(G, _, _)),
    vertices(G, Vs),
    set_infinite_distances(G, Vs),
    retract(distance(G, vertex(G, Source), _)),
    assert(distance(G, vertex(G, Source), 0)),
    set_nil_previous(G, Vs),
    dijkstra_sssp_recursive(G, Source).

dijkstra_sssp(G, Source) :-
    dijkstra_sssp(G, vertex(G, Source)).

dijkstra_sssp_recursive(G, V) :-
    graph(G),
    visited(G, V), !,
    extract(lista, _, U),
    dijkstra_sssp(G, U).

dijkstra_sssp_recursive(G, Start) :-
    graph(G),
    assert(visited(G, Start)),
    new_heap(lista),
    neighbors(G, Start, Neighbors),
    to_vertices(vertex(G, Start), Neighbors, Vertices),
    update_dist_prev(G, vertex(G, Start), Vertices),
    insert_vs(Vertices, lista),
    extract(lista, _, V),
    dijkstra_sssp_recursive(G, V), !.

dijkstra_sssp_recursive(_, _) :-
    empty(lista), !.
%% update_dist_prev/3 - aggiorna le distanze e i vertici precedenti
update_dist_prev(_, _, []).

update_dist_prev(G, V, [U | Vs]) :-
    get_new_dist(G, U, V, NewD),
    distance(G, U, D),
    NewD > D,
    !,
    update_dist_prev(G, V, Vs).

update_dist_prev(G, V, [U | Vs]) :-
    get_new_dist(G, U, V, NewD),
    change_distance(G, U, NewD),
    change_previous(G, U, V),
    update_dist_prev(G, V, Vs).
%% get_new_dist/4 - restituisce la nuova distanza di un vertice
get_new_dist(G, V, Pr, NewW) :-
    graph(G),
    distance(G, Pr, PrDist),
    get_edge_weight(G, Pr, V, Weight),
    NewW is PrDist + Weight.
%% get_edge_weight/4 - restituisce il peso di un arco
get_edge_weight(G, U, V, Weight) :-
    graph(G),
    edge(G, U, V, Weight).

%% to_vertices/3 - trasforma una lista di archi in una lista di vertici
to_vertices(_, [], []).
to_vertices(Source, [edge(G, Source, vertex(G, B), _)|Vs], [vertex(G, B)|Vts]) :-
    vertex(G, B),
    edge(G, Source, vertex(G, B), _),
    to_vertices(Source, Vs, Vts).
%% insert_vs/2 - inserisce i vertici in una coda di priorità
insert_vs([], _).
insert_vs([vertex(G, V) | Vs], H) :-
    vertex(G, V),
    visited(G, V), !,
    insert_vs(Vs, H).
insert_vs([vertex(G, V) | Vs], H) :-
    vertex(G, V),
    distance(G, vertex(G, V), D),
    insert(H, D, V),
    insert_vs(Vs, H).
%%set_infinite_distances/2 - setta le distanze dei vertici a infinito
set_infinite_distances(_, []).

set_infinite_distances(G, [vertex(G, V) | Vs]) :-
    graph(G),
    vertex(G, V),
    assert(distance(G, vertex(G, V), inf)),
    set_infinite_distances(G, Vs).
%% set_nil_previous/2 - setta i vertici precedenti a nil
set_nil_previous(_, []).

set_nil_previous(G, [vertex(G, V) | Vs]) :-
    graph(G),
    vertex(G, V),
    assert(previous(G, vertex(G, V), nil)),
    set_nil_previous(G, Vs).

%% sssp_shortest_path/3 - restituisce il cammino minimo tra due vertici

sssp_shortest_path(G, Source, V, ReversedPath) :-
    ensure_vertex(G, Source, VertexSource),
    ensure_vertex(G, V, VertexV),
    graph(G),
    vertex(G, VertexSource),
    vertex(G, VertexV),
    sssp_shortest_path_aux(G, vertex(G, VertexSource), vertex(G, VertexV), Path),
    reverse(Path, ReversedPath).

sssp_shortest_path_aux(_, Source, Source, []):- !.

sssp_shortest_path_aux(G, vertex(G, Source), vertex(G, V), [edge(G, P, vertex(G, V), Weight) | Path]) :-
    previous(G, vertex(G, V), P),
    edge(G, P, vertex(G, V), Weight),
    sssp_shortest_path_aux(G, vertex(G, Source), P, Path).

%% ensure_vertex/3 - estrae il nome di un vertice da un termine vertex/2 o restituisce il nome stesso
ensure_vertex(G, vertex(G, Source), Source) :- !.
ensure_vertex(_, Source, Source).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Copyright © 2024 TeoMece           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
