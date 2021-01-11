# Graph Algorithms: Minimum Spanning Trees
Lo scopo è implementare l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione **Lisp** e una implementazione **Prolog**, ciascuno con la loro documentation che si può trovare nel file README nella directory corrispondente.

***

## Links
- [Prolog documentation](https://www.swi-prolog.org/pldoc/index.html).
- [Lisp documentation](http://clhs.lisp.se/Front/index.htm).
- [Forum studenti LP](https://elearning.unimib.it/mod/forum/view.php?id=498956).

## To-do
- Mettere il "." alla fine di tutti i predicati del README Prolog (Il prof ha fatto così).
- ? Prolog delete_graph/heap/altro che ritornano sempre true
- Controllare il limite di 80 colonne per tutti i file anche i readme!!
- Controllare predicati / funzioni non utilizzati oppure usati solo una volta
- Ricordarsi che new-arc in lisp ora crea vertici in automatico, disabilitare questa opzione prima della consegna
- Ricordarsi di togliere le negazioni in Prolog che ha detto che è meglio non usarle. Dovrebbero essere tutte negli assert_* e retract_*.
- Eliminare l'mst all'eliminazione del grafo.

## Optimizations
- Fibonacci Heap is better for Prim ([Prim Wikipedia](https://en.wikipedia.org/wiki/Prim%27s_algorithm), [Fibonacci Heap Wikipedia](https://en.wikipedia.org/wiki/Fibonacci_heap)).
- ? Fondere vertex-keys e vertex-previus (per poi interrogare di meno il database).
- ? Controllare ogni archi all'inserimento nell'heap è efficace o non ne vale il tempo speso?
- Sorting personalizzato degli archi anzichè 2 sorting prima su ordine alfabetico e poi peso.
- ? Non memorizzare archi in formato link nei vertici ma in formato più facile da trasformare in rappresentazione standard (?solo Lisp?).
- Controllare dovunque il numero di accessi ai database, diminuirlo quando possibile! (soprattutto per le ricerche, meno per gli accessi mirati)

### Prolog
- Creare il gemello di array-delete-entry per la creazione, per la heap-insert

### Lisp
