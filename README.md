# Graph Algorithms: Minimum Spanning Trees
Goal: calcolare il percorso più breve da un punto a un altro di una mappa.  
Lo scopo sarà raggiunto implementando l'**algoritmo di Prim** per la soluzione del problema **MST** (Minimum Spanning Tree) per grafi indiretti e connessi con pesi non negativi.  
Per questo nel progetto sono presenti, oltre alla parte dedicata alla risoluzione del MST, delle API dedicate alla gestione di **grafi** e **minheap**.  
Il progetto ha una implementazione **Lisp** e una implementazione **Prolog**, ciascuno con la loro documentation che si può trovare nel file README nella directory corrispondente.

***

## Links
- [Prolog documentation](https://www.swi-prolog.org/pldoc/index.html).
- [Forum studenti LP](https://elearning.unimib.it/mod/forum/view.php?id=498956).

## To-do
- Dalla consegna sembra che la heap_extract possa estrarre un elemento qualsiasi, ma in teoria deve essere la head. Vedi il [forum](https://elearning.unimib.it/mod/forum/discuss.php?d=145584#p247226).
- Testare la lettura e scrittura di un file csv con percorsi diversi dalla cartella corrente ([forum](https://elearning.unimib.it/mod/forum/discuss.php?d=146815#p249100)).

### Keep in mind
- Prolog comparison, specialmente per l'ordine lessicografico, a [questa pagina](http://www.cse.unsw.edu.au/~billw/dictionaries/prolog/comparison.html).
- Fibonacci Heap is better for Prim ([Prim Wikipedia](https://en.wikipedia.org/wiki/Prim%27s_algorithm), [Fibonacci Heap Wikipedia](https://en.wikipedia.org/wiki/Fibonacci_heap)).

### Follow
- Dubbio graph_arcs per archi bidirezionali [qui](https://elearning.unimib.it/mod/forum/discuss.php?d=146729#p248947): deve considerare per forza validi tutti i predicati con vertici invertiti?
- Dubbio write_graph edges da una lista di archi di grafi diversi [qui](https://elearning.unimib.it/mod/forum/discuss.php?d=146760#p249007): legale o no?

### Solved
- Un arco del grafo può essere sovrascritto? **=>** deve ([forum](https://elearning.unimib.it/mod/forum/discuss.php?d=142487#p242683)).
- Come verificare che un grafo sia connesso? È necessario farlo? **=>** i grafi possono essere non connessi ([forum](https://elearning.unimib.it/mod/forum/discuss.php?d=146455#p248573)).
