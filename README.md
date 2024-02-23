# Progetto SSSP Prolog

Calcolare il percorso più breve da un punto a un altro di una mappa1 è un problema più che noto. Vi
sono diversi algoritmi in grado di risolvere questo problema, noto in letteratura come il “Single Source
Shortest Path Problem” (SSSP Problem, cfr., [CLR+09] capitolo 24).
Lo scopo di questo progetto è l’implementazione dell’algoritmo di Dijkstra (cfr., [CLR+09] 24.3), che
risolve il problema SSSP per grafi diretti e connessi con distanze tra vertici non negative2

## File

- `graph.pl`: Questo file contiene l'implementazione della struttura dati del grafo e varie operazioni relative al grafo.
- `dijkstra.pl`: Questo file contiene l'implementazione dell'algoritmo di Dijkstra per trovare il percorso più breve in un grafo.
- `minheap.pl`: Questo file contiene l'implementazione di un MinHeap.

## Utilizzo

Per utilizzare l'algoritmo SSSP implementato in questo progetto, segui questi passaggi:

1. Includi i file necessari nel tuo ambiente Prolog.
2. Crea un grafo utilizzando i predicati presenti nel file `graph.pl`.
3. Utilizza il file `dijkstra.pl` per trovare il percorso più breve nel grafo.

### Predicati

Ecco i predicati disponibili nei file:

#### `graph.pl`

- `new_graph/1`: Crea un grafo vuoto.
    ```
    new_graph(g).
    ```
- `new_vertex/2`: Aggiunge un vertice al grafo.
    ```
    new_vertex(g, a).
    ```
- `new_edge/4`: Aggiunge un arco al grafo.
    ```
    new_edge(g, a, b, 3).
    %% oppure
    new_edge(g, vertex(g, a), vertex(g, b), 3).
    ```
- 'delete_graph/1' : Elimina un grafo.
    ```
    delete_graph(g).
    ```
- `vertices/2`: Ha successo se la lista Vs è la lista di tutti i vertici del grafo g.
    ```
    vertices(g, Vs).
    ```
- 'list_vertices/1': Stampa i vertici del grafo
    ```
    list_vertices(g).
    ```
- `edges/2`: Ha successo se la lista Es è la lista di tutti gli archi del grafo g.
    ```
    edges(g, Es).
    ```
- 'list_edges/1': Stampa gli archi di un grafo
    ```
    list_edges(g).
    ```
- `neighbors/3`: Ha successo se Ns è la lista degli archi uscenti dal vertice a nel grafo g.
    ```
    neighbors(g, a, Ns).
    ```

#### `dijkstra.pl`
- `dijkstra_sssp/2`: Esegue l'algoritmo di Dijkstra per calcolare le distanze minime tra la sorgente e tutti i nodi del grafo.
    ```
    dijkstra_sssp(g, source).
    ```
- `sssp_shortest_path/4`: Ha successo se la lista di archi passata come ultimo argomento che rappresenta il cammino minimo dalla         sorgente al nodo x.
    ```
    sssp_shortest_path(g, source, x, P).
    ```

#### 'minheap.pl'
- `new_heap/1`: Crea un heap vuoto.
    ```
    new_heap(h).
    ```
- `heap_size/2`: Ha successo se il secondo argomento è la dimensione dell'heap.
    ```
    heap_size(g).
    ```
- `delete_heap/1`: Elimina un heap.
    ```
    delete_heap(h).
    ```
- `empty/1`: Ha successo se l'heap è vuoto.
    ```
    empty(h).
    ```
- `not_empty/1`: Ha successo se l'heap non è vuoto.
    ```
    not_empty(h).
    ```
- `head/2`: Ha successo se l'argomento è la testa dell'heap.
    ```
    head(h).
    ```
- `insert/3`: inserisce una coppia chiave-valore nell'heap. ///////////////////////
    ```
    insert(h, k, v).
    ```
- `extract/3`: Ha successo solo se la coppia chiave-valore passata è la testa dell'heap.
    ```
    extract(h, k, v).
    ```
- `modify_key/4`: Modifica la chiave associata ad un elemento dell'heap.
    ```
    modify_key(h, newK, oldK, v).
    ```
- `list_heap/1`: Stampa la lista degli elementi presenti nell'heap.
    ```
    list_heap(h).
    ```
- `min_key/4`: Ha successo se m è la chiave più piccola dell'heap nell'intervallo di posizioni s-e.
    ```
    min_key(h, s, e, m).
    ```