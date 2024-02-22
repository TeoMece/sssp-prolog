# Progetto SSSP Prolog

Questo progetto è una raccolta di file che implementano un algoritmo SSSP (Single-Source Shortest Path) basato su Prolog.

## File

- `graph.pl`: Questo file contiene l'implementazione della struttura dati del grafo e varie operazioni relative al grafo.
- `dijkstra.pl`: Questo file contiene l'implementazione dell'algoritmo di Dijkstra per trovare il percorso più breve in un grafo.
- `minheap.pl`: Questo file contiene l'implementazione di un MinHeap.
- `README.md`: Questo file fornisce una panoramica del progetto e dei suoi file (escludendo i file di test).

## Utilizzo

Per utilizzare l'algoritmo SSSP implementato in questo progetto, segui questi passaggi:

1. Includi i file necessari nel tuo ambiente Prolog.
2. Crea un grafo utilizzando il file `graph.pl`.
3. Utilizza il file `dijkstra.pl` per trovare il percorso più breve nel grafo.

### Predicati

Ecco i predicati disponibili nei file:

#### `graph.pl`

- `new_graph/1`: Crea un grafo vuoto.
- `new_vertex/2`: Aggiunge un vertice al grafo.
- `new_edge/4`: Aggiunge un arco al grafo.
- 'delete_graph/1' : Elimina un grafo.
- `remove_edge/4`: Rimuove un arco dal grafo.
- `vertices/2`: Restituisce la lista dei vertici nel grafo.
- 'list_vertices/1': Stampa i vertici del grafo
- `edges/2`: Restituisce la lista degli archi nel grafo.
- 'list_edges/1': Stampa gli archi di un grafo
- `neighbors/3`: Restituisce la lista degli archi uscenti da un vertice nel grafo.

#### `dijkstra.pl`

#### 'minheap.pl'
