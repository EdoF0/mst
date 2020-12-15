# Graph Algorithms: Minimum Spanning Trees
Goal: calcolare il percorso più breve da un punto a un altro di una mappa.  
Lo scopo sarà raggiunto implementando l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione Lisp e una implementazione Prolog, questo è il documento per la implementazione **Prolog**.

***

# documentazione MST Prolog

## MST
Il MST (Minimum Spanning Tree) anche detto in italiano albero ricoprente minimo di un grafo, è l'albero nel quale sommando i pesi degli archi si ottiene il valore minimo e che tocca tutti i vertici del grafo di partenza.  
L'algoritmo usato in questa libreria è l'algoritmo di Prim, noto anche con altri nomi come l'algoritmo DJP, è un algoritmo che ha origine nel 1930 ed è veloce, efficiente e relativamente semplice.  
Il funzionamento dell'algoritmo nella sua essenza è che, a partire da un nodo arbitrario, fino al raggiungimento di tutti i vertici raggiungibili, ad ogni passo verifica il nodo più vicino all'albero finora costruito e lo inserisce nel MST.

### Esecuzione

#### mst_prim
```prolog
mst_prim(G, Source)
```
Calcola l'MST del grafo G a partire dal vertice Source, e aggiungi il risultato alla base dati Prolog.

#### mst_get
```prolog
mst_get(G, Source, PreorderTree)
```
Vero quando PreorderTree è la lista degli archi MST generati con `mst_prim(G, Source)`.  
PreorderTree è generata secondo una visita anticiapta (anche detta visita in preordine o pre-order tree trasversal) del MST, nei livelli ordinata secondo il peso dell'arco e, in caso di parità, in ordine lessicografico.

### Dati

#### vertex_key
```prolog
vertex_key(G, V, K)
```
Vero quando V fa parte del grafo G durante e dopo il calcolo di un MST, e K è il peso minimo di un arco di V che lo collega al MST.  
K vale `inf` se V non è parte del MST, succede solo nei grafi non connessi.  
Viene valutato falso se V non è (ancora) parte del MST.

#### vertex_previous
```prolog
vertex_previous(G, V, U)
```
Vero quando V fa parte del grafo G durante e dopo il calcolo di un MST, e U è il genitore (parent) di V nel MST.  
Viene valutato falso se V non è (ancora) parte del MST.

***

## MinHeap
Libreria dedicata alla gestione di più minheap.  
Tutte le operazioni di modifica includono la riorganizzazione interna della struttura dati.

### Creazione e modifica

#### new_heap
```prolog
new_heap(H)
````
Crea un nuovo minheap H se non esiste già. Non può fallire.

#### delete_heap
```prolog
delete_heap(H)
```
Elimina il minheap H se esiste, altrimenti fallisce.

#### heap_insert
```prolog
heap_insert(H, K, V)
```
Inserisce nel minheap H un valore V con chiave K.  
Fallisce se il minheap H non esiste o se la chiave K non è un intero.

#### heap_extract
```prolog
heap_extract(H, K, V)
```
Elimina dal minheap H il valore V con chiave K.  
Fallisce se non esistono il minheap H oppure l'elemento indicato.  
In caso di K e o V uguali per più elementi, viene eliminato il primo nell'ordine della base di conoscenza Prolog, ovvero il primo inserito.

#### modify_key
```prolog
modify_key(H, NewKey, OldKey, V)
```
Sostituisce al valore V nel minheap H la chiave OldKey con la chiave NewKey.  
Fallisce se non esistono il minheap o l'elemento indicato.

### Lettura

#### heap_has_size
```prolog
heap_has_size(H, S)
```
Vero se il minheap H ha dimensione S, cioè ha S elementi.

#### heap_empty
```prolog
heap_empty(H)
```
Vero se il minheap H è privo di elementi.

#### heap_not_empty
```prolog
heap_not_empty(H)
```
Vero se il minheap H contiene almeno un elemento.

#### heap_head
```prolog
heap_head(H, K, V)
```
Vero se l'elemento minimo del minheap H è quello con chiave K e valore V.

### Stampa

#### list_heap
```prolog
list_heap(H)
```
Stampa gli elementi del minheap H con il predicato speciale listing (in ordine di inserimento nella base di conoscenza Prolog).

***

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

### Lettura

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
Stampa i vertici del grafo G con il predicato speciale listing (in ordine di inserimento nella base di conoscenza Prolog).

#### list_arcs
```prolog
list_arcs(G)
```
Stampa gli archi del grafo G con il predicato speciale listing (in ordine di inserimento nella base di conoscenza Prolog).

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
