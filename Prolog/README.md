# Graph Algorithms: Minimum Spanning Trees
Lo scopo è implementare l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione Lisp e una implementazione Prolog, questo è il documento per la implementazione **Prolog**.

***

# documentazione MST Prolog

## MST
Il MST (Minimum Spanning Tree, in italiano albero ricoprente minimo di un grafo) è l'albero che tocca tutti i vertici del grafo di partenza nel quale la somma dei pesi degli archi è minima.  
L'algoritmo usato in questa libreria è l'algoritmo di Prim (noto anche come l'algoritmo di Jarník), è un algoritmo che ha origine nel 1930 ed è veloce, efficiente e relativamente semplice.  
È possibile tenere in memoria un solo MST per grafo.

### Esecuzione

#### mst_prim
```prolog
mst_prim(G, Source)
```
Calcola il MST del grafo G a partire dal vertice Source, e aggiunge il risultato alla base dati Prolog.  
È possibile avere al massimo un MST per grafo, il tentativo di calcolarne altri sovrascriverà i precedenti.  
Fallisce se il grafo G non esiste o Source non è un vertice di G.  
Nota: durante la esecuzione è usato un heap nominato G, che verrà sovrascritto se già presente, perciò non usate un heap che si chiama come il grafo di cui volete calcolare il MST!

#### mst_get
```prolog
mst_get(G, Source, PreorderTree)
```
Vero quando PreorderTree è la lista degli archi MST generati con `mst_prim(G, Source)`.  
PreorderTree è generata secondo una visita anticiapta (anche detta visita in preordine o pre-order tree trasversal) del MST ordinata secondo il peso dell'arco e, in caso di parità, in ordine lessicografico.

### Dati

#### vertex_key
```prolog
vertex_key(G, V, K)
```
Vero quando V fa parte del grafo G e K è il peso minimo di un arco di V appartente al MST di G.  
K vale `inf` se V non è parte del MST, succede solo nei grafi non connessi.  
Viene valutato falso se V non è (ancora) parte del MST di G.

#### vertex_previous
```prolog
vertex_previous(G, V, U)
```
Vero quando V fa parte del grafo G e U è il genitore di V nel MST di G.  
Viene valutato falso se V non è (ancora) parte del MST di G, oppure se V è la radice, oppure se V non fa parte della componente del grafo G connessa alla radice del MST di G.

***

## Grafi
Libreria dedicata alla gestione di più grafi indiretti con pesi non negativi.

### Creazione e modifica

#### new_graph
```prolog
new_graph(G)
```
Crea un nuovo grafo G, se G esiste già viene sovrascritto. Fallisce se G è una variabile.

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
Fallisce se non esiste G, oppure V è una variabile.

#### new_arc
```prolog
new_arc(G, U, V, W)
new_arc(G, U, V) ≡ new_arc(G, U, V, 1)
```
Crea un nuovo arco da U a V con peso W nel grafo G se non esiste già.  
Fallisce se non esiste G o U o V, oppure se W non è un numero non negativo (sono accettati numeri razionali).  
Creare un arco già esistente con peso diverso sovrascrive quello precedente.

### Lettura

#### graph_vertices
```prolog
graph_vertices(G, Vs)
```
Vero se Vs è la lista di tutti i vertici di G.  
Se G non esiste Vs è la lista vuota.  
La lista è composta da elementi `vertex(G, vertice)`.

#### graph_arcs
```prolog
graph_arcs(G, As)
```
Vero se Es è la lista di tutti gli archi di G.  
Se G non esiste As è la lista vuota.  
La lista è composta da elementi `arc(G, vertce1, vertice2, peso)`.

#### vertex_neighbors
```prolog
vertex_neighbors(G, V, As)
```
Vero se As è la lista di tutti gli archi collegati al vertice V del grafo G.

#### adjs
```prolog
adjs(G, V, Vs)
```
Vero se Vs è la lista di tutti i vertici adiacenti al vertice V del grafo G.

### Stampa

#### list_vertices
```prolog
list_vertices(G)
```
Stampa i vertici del grafo G.

#### list_arcs
```prolog
list_arcs(G)
```
Stampa gli archi del grafo G.

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
Se il grafo G esiste già, verrà sovrascritto.  
In caso di errore nella sintassi o nel contenuto del file il predicato verrà valutato come falso, ma tutto ciò che precede l'errore nel file csv verrà aggiunto al grafo G normalmente.
Fallisce se G o FileName sono variabili.

#### write_graph
```prolog
write_graph(G, FileName, graph | edges)
write_graph(G, FileName) ≡ write_graph(G, FileName, graph)
```
Crea un file csv FileName a partire da un grafo G se il terzo argomento è 'graph', a partire da una lista di archi se il terzo argomento è 'edges'.  
La sintassi del file csv è la stessa di read_graph.  
La sintassi della lista di archi è la stessa di graph_arcs.  
Fallisce se G o FileName sono variabili, oppure se il terzo argomento è diverso da "graph" o "edges".

***

## MinHeap
Libreria dedicata alla gestione di più binary minheap.  
Tutte le operazioni di modifica includono la riorganizzazione interna della struttura dati.

### Creazione e modifica

#### new_heap
```prolog
new_heap(H)
````
Crea un nuovo minheap H, se H esiste già viene sovrascritto. Fallisce se H è una variabile.

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
Fallisce se non esiste H, oppure se la chiave K non è un intero, oppure se K o V sono variabili.

#### heap_extract
```prolog
heap_extract(H, K, V)
```
Elimina dal minheap H il valore V con chiave K. K e V possono essere definiti oppure lasciati entrambi variabili per una operazione di extract standard sulla testa del minheap.  
Fallisce se non esiste H o V.  
In caso di K e o V uguali per più elementi, viene eliminato il primo nell'ordine della base di conoscenza Prolog, ovvero il primo inserito.

#### modify_key
```prolog
modify_key(H, NewKey, OldKey, V)
```
Sostituisce al valore V nel minheap H la chiave OldKey con la chiave NewKey.  
Fallisce se non esiste H o V, oppure se NewKey è una variabile.

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
Stampa gli elementi del minheap H.