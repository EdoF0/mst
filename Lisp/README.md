# Graph Algorithms: Minimum Spanning Trees
Lo scopo è implementare l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione Lisp e una implementazione Prolog, questo è il documento per la implementazione **Lisp**.

***

# documentazione MST Lisp

## MST
Il MST (Minimum Spanning Tree, in italiano albero ricoprente minimo di un grafo) è l'albero che tocca tutti i vertici del grafo di partenza nel quale la somma dei pesi degli archi è minima.  
L'algoritmo usato in questa libreria è l'algoritmo di Prim (noto anche come l'algoritmo di Jarník), è un algoritmo che ha origine nel 1930 ed è veloce, efficiente e relativamente semplice.  
*È possibile tenere in memoria un solo MST per grafo.*

### Esecuzione

#### mst-prim
```lisp
mst-prim graph-id source => NIL
```
Calcola il MST del grafo graph-id a partire dal vertice source, e memorizza il risultato.  
*È possibile avere al massimo un MST per grafo, il tentativo di calcolarne altri sovrascriverà i precedenti.*  
*Fallisce silenziosamente se il grafo graph-id non esiste o source non è un vertice di graph-id.*  
Nota: durante la esecuzione è usato un heap nominato graph-id, che verrà sovrascritto se già presente, perciò non usate un heap che si chiama come il grafo di cui volete calcolare il MST!

#### mst-get
```lisp
mst-get graph-id source => preorder-mst
```
Restituisce la lista degli archi MST generati con `(mst-prim graph-id mst-source)` a partire dal vertice source.  
preorder-mst è generato secondo una visita anticiapta (anche detta visita in preordine o pre-order tree trasversal) del MST ordinata secondo il peso dell'arco e, in caso di parità, in ordine lessicografico.

### Dati

#### mst-vertex-key
```lisp
mst-vertex-key graph-id vertex-id => k
```
Restituisce il peso minimo k di un arco entrante o uscente a vertex-id appartente al MST di graph-id.  
k vale `most-positive-double-float` se vertex-id non è parte (ancora) del MST.

#### mst-previous
```lisp
mst-previous graph-id vertex-id => parent-vertex-id
```
Restituisce il genitore parent-vertex-id di vertex-id nel MST di graph-id.  
V vale NIL se vertex-id non è (ancora) parte del MST di graph-id, oppure se vertex-id è la radice, oppure se vertex-id non fa parte della componente del grafo graph-id connessa alla radice del MST.

***

## Grafi
Libreria dedicata alla gestione di più grafi indiretti con pesi non negativi.

### Creazione e modifica

#### new-graph
```lisp
new-graph graph-id => graph-id
```
Crea un nuovo grafo graph-id, *se graph-id esiste già viene sovrascritto*.

#### delete-graph
```lisp
delete-graph graph-id => NIL
```
Elimina il grafo graph-id se esiste.

#### is-graph
```lisp
is-graph graph-id => graph-id
```
Ritorna graph-id se graph-id è un grafo che esiste, NIL altrimenti.

#### new-vertex
```lisp
new-vertex graph-id vertex-id => vertex-rep
```
Crea un nuovo vertice vertex-id nel grafo graph-id *se non esiste già*.  
Ritorna il vertice rappresentato come `(vertex graph-id vertex-id)`.  
Ritorna NIL se non esiste graph-id.

#### new-arc
```lisp
new-arc graph-id vertex-id vertex-id &optional (weight 1) => arc-rep
```
Crea un nuovo arco dal primo vertex-id al secondo peso weight nel grafo graph-id se non esiste già.  
Ritorna l'arco rappresentato come `(arc graph-id vertex-id vertex-id weight)`.  
Ritorna NIL se non esiste graph-id o i vertici specificati, oppure se weight non è un numero non negativo (sono accettati numeri razionali).  
*Creare un arco già esistente con peso diverso sovrascrive quello precedente.*

### Lettura

#### graph-vertices
```lisp
graph-vertices graph-id => vertex-rep-list
```
Ritorna la lista di tutti i vertici di graph-id.  
Se graph-id non esiste la lista è vuota.  
La lista è composta da elementi vertex-rep, come ritornato dalla new-vertex.

#### graph-arcs
```lisp
graph-arcs graph-id => arc-rep-list
```
Ritorna la lista di tutti gli archi di graph-id.  
Se graph-id non esiste la lista è vuota.  
La lista è composta da elementi arc-rep, come ritornato dalla new-arc.

#### graph-vertex-neighbors
```lisp
graph-vertex-neighbors graph-id vertex-id => arc-rep-list
```
Ritorna la lista (arc-rep) di tutti gli archi collegati al vertice vertex-id del grafo graph-id.

#### graph-vertex-adjacent
```lisp
graph-vertex-adjacent graph-id vertex-id => vertex-rep-list
```
Ritorna la lista (vertex-rep) di tutti i vertici adiacenti al vertice vertex-id del grafo graph-id.

### Stampa

#### list-graph
```lisp
graph-print graph-id => NIL
```
Stampa i vertici e gli archi del grafo G.

***

## MinHeap
Libreria dedicata alla gestione di più binary minheap.  
Tutte le operazioni di modifica includono la riorganizzazione interna della struttura dati.

### Creazione e modifica

#### new-heap
```lisp
new-heap heap-id &optional (capacity 42) => heap-rep
```
Crea un nuovo minheap heap-id, *se heap-id esiste già viene sovrascritto*.  
Ritorna l'heap rappresentato come `(heap heap-id heap-size actual-heap)`.

#### heap-delete
```lisp
heap-delete heap-id => T
```
Elimina il minheap heap-id se esiste.  
*?Perchè cavolo heap-delete e non delete-heap come finora, e poi perchè ritorna T se delete-graph ritorna NIL?*

#### heap-insert
```lisp
heap-insert heap-id key value => boolean
```
Inserisce nel minheap heap-id un valore value con chiave key.  
Fallisce se è esaurita la dimensione dell'heap.

#### heap-extract
```lisp
heap-extract heap-id => (key value)
```
Elimina dal minheap heap-id la testa.  
Ritorna la chiave key e il valore value della testa rimossa.

#### heap-modify-key
```lisp
heap-modify-key heap-id new-key old-key value => boolean
```
Sostituisce la chiave dell'elemento con valore value e chiave OldKey con il nuovo valore NewKey.  
Ritorna NIL solo se *bho*.

### Lettura

#### hea-empty
```lisp
heap-empty heap-id => boolean
```
Ritorna T se il minheap heap-id è privo di elementi, NIL altrimenti.

#### heap-not-empty
```lisp
heap-not-empty heap-id => boolean
```
Ritorna T se il minheap heap-id contiene almeno un elemento, NIL altrimenti.

#### heap-head
```lisp
heap-head heap-id => (key value)
```
Ritorna la chiave key e il valore v dell'elemento in testa al minheap heap-id.

### Stampa

#### heap-print
```lisp
heap-print heap-id => boolean
```
Stampa gli elementi del minheap heap-id *in ordine di chiave*, *ritorna NIL solo se heap-id non esiste*.