crea_grafo_prova(G) :-
    new_graph(G),
    new_vertex(G, v),
    new_vertex(G, u),
    new_vertex(G, x),
    new_vertex(G, y),
    new_edge(G, v, u, 2),
    new_edge(G, v, y, 3),
    new_edge(G, y, x, 1).
