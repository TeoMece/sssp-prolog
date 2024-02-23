create_graph(G) :-
    new_graph(G),
    new_vertex(G, source),
    new_vertex(G, x),
    new_vertex(G, y),
    new_vertex(G, z),
    new_vertex(G, t),
    new_edge(G, source, y, 5),
    new_edge(G, source, t, 10),
    new_edge(G, t, x, 1),
    new_edge(G, t, y, 2),
    new_edge(G, y, t, 3),
    new_edge(G, y, x, 9),
    new_edge(G, y, z, 2),
    new_edge(G, z, source, 7),
    new_edge(G, z, x, 6),
    new_edge(G, x, z, 4),
    list_graph(G).

%-----% HEAP %-----%
create_heap(H) :-
    new_heap(H),
    insert(H, 7, a),
    insert(H, 49, b),
    insert(H, 3, c),
    insert(H, 15, d),
    insert(H, 2, e),
    insert(H, 6, f),
    insert(H, 1, g),
    insert(H, 10, h),
    insert(H, 15, i),
    insert(H, 14, j),
    insert(H, 5, k),
    insert(H, 99, l),
    list_heap(H).

%-----% DIJKSTRA E SHORTEST_PATH %-----%
test_shortest_path(G) :-
    create_graph(G),
    shortest_path(G, source, x,
                  [edge(G, source, y, _),
                   edge(G, y, t, _),
                   edge(G, t, x, _)]).


