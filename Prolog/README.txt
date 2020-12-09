Readme Prolog

# Graph Algorithms: Minimum Spanning Trees
goal: calcolare il percorso più breve da un punto a un altro di una mappa.  
Lo scopo sarà raggiunto implementando l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione Lisp e una implementazione Prolog, questo è il documento per la implementazione **Prolog**.

***

# documentazione MST Prolog

## Grafi
Libreria dedicata alla gestione di più grafi indiretti e connessi con pesi non negativi.

### Creazione e modifica

#### new_graph
```prolog
new_graph(G)
```
Crea un nuovo grafo G se non esiste già. Non può fallire.

#### delete_graph
```prolog
delete_graph(G)
```
Elimina il grafo G se esiste, altrimenti fallisce.

#### new_vertex
```prolog
new_vertex(G, V)
```
Crea un nuovo vertice V nel grafo G se non esiste già.  
Fallisce seil grafo G non esiste.

#### new_arc
```prolog
new_arc(G, U, V, W)
new_arc(G, U, V) ≡ new_arc(G, U, V, 1)
```
Crea un nuovo arco da U a V con peso W nel grafo G se non esiste già.  
Fallisce se il grafo G non esiste oppure se uno dei due vertici U e V non esistono nel grafo G. Fallisce anche se il peso W non è un numero positivo (sono accettati numeri razionali).  
Un arco già esistente con peso diverso sovrascrive quello precedente.

### Lettura dei grafi

#### graph_vertices
```prolog
graph_vertices(G, Vs)
```
Vero se Vs è la lista di tutti i vertici di G.  
La lista è composta da elementi `vertex(G, vertice)`.

#### graph_arcs
```prolog
graph_arcs(G, Es)
```
Vero se Es è la lista di tutti gli archi di G.  
La lista è composta da elementi `arc(G, vertce1, vertice2, peso)`.

#### vertex_neighbors
```prolog
vertex_neighbors(G, V, Ns)
```
Vero se Ns è la lista di tutti gli archi collegati al vertice V del grafo G.

#### adjs
```prolog
adjs(G, V, Vs)
```
Vero se Vs è la lista di tutti i vertici adiacenti (collegati da un solo arco) ad il vertice V del grafo G.

### Stampa

#### list_vertices
```prolog
list_vertices(G)
```
Stampa i vertici del grafo G con il predicato speciale listing.

#### list_arcs
```prolog
list_arcs(G)
```
Stampa gli archi del grafo G con il predicato speciale listing.

#### list_graph
```prolog
list_graph(G)
```
Stampa i vertici e gli archi del grafo G.

### File csv

#### read_graph
```prolog
read_graph(G, FileName)
```
Crea un grafo G a partire da un file csv FileName con carattere separatore tabulazione, dove ogni riga è composta da vertce 1, vertice 2 e peso dell'arco dal vertice 1 al vertice 2.  
Se il grafo G esiste già gli elementi del file verranno aggiunti al grafo G esistente.  
In caso di errore nella sintassi o nel contenuto del file il predicato verrà valutato come falso, ma tutto ciò che precede l'errore nel file csv verrà aggiunto al grafo G normalmente.

#### write_graph
```prolog
write_graph(G, FileName, graph | edges)
write_graph(G, FileName) ≡ write_graph(G, FileName, graph)
```
Crea un file csv FileName a partire da un grafo G se il terzo argomento è 'graph', a partire da una lista di archi se il terzo argomento è 'edges'.  
La sintassi del file csv è la stessa di read_graph.  
La sintassi della lista di archi è la stessa di graph_arcs.
