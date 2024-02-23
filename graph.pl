%%% Interfaccia Prolog per la manipolazione di grafi
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.
%%% new_graph/1 Inizializzazione del grafo vuoto

new_graph(G) :- 
    atom(G),
    graph(G),
    !.
new_graph(G) :- 
    atom(G),
    assert(graph(G)),
    !.

%%% delete_graph/1 Eliminazione del grafo
delete_graph(G) :- 
    atom(G),
    retract(graph(G)),
    retractall(vertex(G, _)),
    retractall(edge(G, _, _, _)),
    !.

%%% new_vertex/2 Aggiunta di un vertice al grafo

new_vertex(G, V) :- 
    atom(G),
    atom(V),
    graph(G),
    vertex(G, V),
    !.
new_vertex(G, V) :- 
    atom(G),
    atom(V),
    graph(G),
    assert(vertex(G, V)),
    !.

%% vertices /2 Restituisce la lista dei vertici del grafo 
vertices(G, Vs) :- 
    atom(G),
    graph(G),
    findall(vertex(G,V), vertex(G, V), Vs),
    !.

%% list_vertices /1 Stampa la lista dei vertici del grafo
list_vertices(G) :-
    atom(G),
    listing(vertex(G, _)),
    !.

%%% new_edge/3 Aggiunta di un arco al grafo
new_edge(G, V1, V2) :- 
    new_edge(G, V1, V2, 1),
    !.

new_edge(G, vertex(G, V1), vertex(G, V2), W) :- %% Se esiste non faccio niente
    edge(G, vertex(G,V1), vertex(G,V2), W),
    !.
new_edge(G, vertex(G, V1), vertex(G, V2), W) :-
    graph(G),
    integer(W),
    vertex(G, V1),
    vertex(G, V2),
    edge(G, vertex(G,V1), vertex(G,V2), _),
    !,
    fail.

new_edge(G, vertex(G, V1), vertex(G, V2), W) :-
    graph(G),
    vertex(G, V1),
    vertex(G, V2),
    W >= 0,
    assert(edge(G, vertex(G,V1), vertex(G,V2), W)),
    !.

new_edge(G, V1, V2, W) :- %% Se esiste non faccio niente
    edge(G, vertex(G,V1), vertex(G,V2), W),
    !.

new_edge(G, V1, V2, W) :- 
    new_edge(G, vertex(G, V1), vertex(G, V2), W).

%% edges /2 Restituisce la lista degli archi del grafo
edges(G, Es) :- 
    atom(G),
    var(Es),
    graph(G),
    findall(edge(G, V1, V2, W), edge(G, V1, V2, W), Es),
    !.

%% neighbors /3 Restituisce la lista dei vicini di un vertice
neighbors(G, vertex(G,V), Ns) :- 
    atom(G),
    var(Ns),
    graph(G),
    findall(edge(G, vertex(G,V), N, W), edge(G, vertex(G,V), N, W), Ns),
    !.
neighbors(G, V, Ns) :- 
    atom(G),
    var(Ns),
    graph(G),
    findall(edge(G, vertex(G,V), N, W), edge(G, vertex(G,V), N, W), Ns),
    !.

%%% list_edges /1 Stampa la lista degli archi del grafo
list_edges(G) :- 
    atom(G),
    graph(G),
    listing(edge(G, _, _, _)),
    !.

%%% list_graph /1 Stampa il grafo
list_graph(G) :- 
    atom(G),
    graph(G),
    list_vertices(G),
    list_edges(G),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Copyright © 2024 TeoMece           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%