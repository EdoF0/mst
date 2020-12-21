

% graph


% creation and edit
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)).

delete_graph(G) :- graph(G), retract(graph(G)),
    retractall(vertex(G, _)),
    retractall(arc(G, _, _, _)).

new_vertex(G, V) :- graph(G), vertex(G,V), !.
new_vertex(G, V) :- graph(G), assert(vertex(G, V)).

new_arc(G, U, V, W) :- graph(G), vertex(G, V),
    vertex(G, U), check_arc(G, U, V, W), !.
new_arc(G, U, V, W) :- number(W), W >= 0,
    graph(G), vertex(G, V),
    vertex(G, U), check_arc(G, U, V, W2),
    delete_arc(G, U, V, W2),
    assert(arc(G, U, V, W)), !.
new_arc(G, U, V, W) :- number(W), W >= 0,
    graph(G), vertex(G, V),
    vertex(G, U), assert(arc(G, U, V, W)).
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% reading
graph_vertices(G, Vs) :- findall(vertex(G, V), vertex(G, V), Vs).

graph_arcs(G, Es) :- findall(arc(G, U, V, W), arc(G, U, V, W), Es).

vertex_neighbors(G, V, Ns) :- vertex(G, V),
    findall(arc(G, V, N, W), check_arc(G, V, N, W), Ns).

adjs(G, V, Vs) :- findall(vertex(G, U), check_arc(G, V, U, _), Vs).

% print
list_vertices(G) :- graph(G), listing(vertex(G, _)).

list_arcs(G) :- graph(G), listing(arc(G, _U, _V, _W)).

list_graph(G) :- graph(G), list_vertices(G), list_arcs(G).

% csv file
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows,
        [separator(0'\t),
        functor(generic_arc),
        arity(3)]),
    new_graph(G), add_from_ga(G, Rows).

write_graph(G, FileName, graph) :- graph_arcs(G, As),
    write_graph(As, FileName, edges), !.
write_graph(As, FileName, edges) :-
    arcs_to_gas(As, Gas),
    csv_write_file(FileName, Gas,
        [functor(generic_arc), arity(3),
        separator(0'\t)]), !.
write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

% support
check_arc(G, U, V, W) :- arc(G, U, V, W).
check_arc(G, U, V ,W) :- arc(G, V, U, W).

delete_arc(G, U, V, W) :- retract(arc(G, U, V, W)).
delete_arc(G, U, V, W) :- retract(arc(G, V, U, W)).

add_from_ga(_, []) :- !.
add_from_ga(G, [generic_arc(V, U, W) | Ls]) :-
    new_vertex(G, V), new_vertex(G, U),
    new_arc(G, V, U, W), add_from_ga(G, Ls).

arc_to_ga(arc(_, V, U, W), generic_arc(V, U, W)).

arcs_to_gas([], []).

arcs_to_gas([A | As], [Ga | Gas]) :-
arc_to_ga(A, Ga), arcs_to_gas(As, Gas).


% minheap


% creation and edit
:- dynamic heap/2.
:- dynamic heap_entry/4.

new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

delete_heap(H) :- retractall(heap_entry(H, _P, _K, _V)), retract(heap(H, _S)).

heap_insert(H, K, V) :- heap(H, S), S > 0, !, integer(K),
    P is S+1,
    assert(heap_entry(H, P, K, V)),
    heap_increment(H), heapify_up(H, P).
heap_insert(H, K, V) :- heap(H, 0), !, integer(K),
    assert(heap_entry(H, 1, K, V)),
    heap_increment(H).

heap_extract(H, K, V) :- heap(H, _), var(K), var(V), !,
    heap_head(H, K, V), heap_extract(H, K, V).
heap_extract(H, K, V) :- heap(H, S), heap_entry(H, P, K, V),
    P < S, !,
    retract(heap_entry(H, P, K, V)), retract(heap_entry(H, S, K1, V1)),
    assert(heap_entry(H, P, K1, V1)),
    heap_decrement(H), heapify(H, P), heapify_up(H, P).
heap_extract(H, K, V) :- heap(H, S), heap_entry(H, P, K, V),
    P = S, !,
    retract(heap_entry(H, P, K, V)),
    heap_decrement(H).

modify_key(H, NewKey, OldKey, V) :- heap(H, _),
    retract(heap_entry(H, P, OldKey, V)), assert(heap_entry(H, P, NewKey, V)),
    heapify(H, P), heapify_up(H, P).

% reading
heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap(H, S), S =< 0.

heap_not_empty(H) :- heap(H, S), S > 0.

heap_head(H, K, V) :- heap_entry(H, 1, K, V).

% print
list_heap(H) :- heap(H, _S), listing(heap_entry(H, _P, _K, _V)).

% support
heap_increment(H) :- heap(H, S),
    retract(heap(H,S)), Sn is S+1, assert(heap(H, Sn)), !.

heap_decrement(H) :- heap(H, S), S > 0,
    retract(heap(H,S)), Sn is S-1, assert(heap(H, Sn)), !.

buildheap(H) :- heap(H, S), Sn is floor(S/2), buildheap(H, Sn).
buildheap(_H, 0) :- !.
buildheap(H, S) :- heapify(H, S), Sn is S-1, buildheap(H, Sn).

heap_entry_left(H, P, Pl) :- heap(H, S), P >= 1, P =< S, Pl is P*2, Pl =< S.
heap_entry_right(H, P, Pr) :- heap(H, S), P >= 1, P =< S, Pr is P*2+1, Pr =< S.
heap_entry_parent(H, P, Pp) :- heap(H, S), P >= 1, P =< S, Pp is floor(P/2), Pp >= 1.

swap_heap_entries(H, P1, P2) :-
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    assert(heap_entry(H, P1, K2, V2)).

heapify_up(H, P) :- heap_entry_parent(H, P, Pp),
    heap_entry(H, P, K, _), heap_entry(H, Pp, Kp, _),
    Kp =< K, !.
heapify_up(H, P) :- heap_entry_parent(H, P, Pp),
    heap_entry(H, P, K, _), heap_entry(H, Pp, Kp, _),
    K < Kp, !,
    swap_heap_entries(H, P, Pp), heapify_up(H, Pp).
heapify_up(H, P) :- heap(H, _S), P =< 1, !.

heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K, _), heap_entry(H, Pl, Kl, _), heap_entry(H, Pr, Kr, _),
    K =< Kl, K =< Kr, !.
heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K, _), heap_entry(H, Pl, Kl, _), heap_entry(H, Pr, Kr, _),
    Kl =< K, Kl =< Kr, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K, _), heap_entry(H, Pl, Kl, _), heap_entry(H, Pr, Kr, _),
    Kr < K, Kr < Kl, !,
    swap_heap_entries(H, P, Pr), heapify(H, Pr).
heapify(H, P) :- heap_entry_left(H, P, Pl),
    heap_entry(H, P, K, _), heap_entry(H, Pl, Kl, _),
    K =< Kl, !.
heapify(H, P) :- heap_entry_left(H, P, Pl),
    heap_entry(H, P, K, _), heap_entry(H, Pl, Kl, _),
    Kl < K, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap(H, S), Not_leaves is floor(S/2)+1, P >= Not_leaves, !.


% mst


% execution
mst_prim(G, Source) :- delete_mst(G), new_graph(G), new_heap(G),
    new_vertex_key(G, Source, inf), heap_add_arcs(G, Source), mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), vertex_key(G, V, _), !, heap_extract(G, W, A), mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), !, heap_extract(G, W, A), new_vertex_key(G, V, W),
    new_vertex_previous(G, V, U), new_vertex_key(G, U, W),
    heap_add_arcs(G, V), mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, V, U, W],
    vertex_key(G, U, _), !, heap_extract(G, W, A), new_vertex_key(G, V, W),
    new_vertex_previous(G, V, U), new_vertex_key(G, U, W),
    heap_add_arcs(G, V), mst_prim(G).
mst_prim(G) :- heap_empty(G), !.

mst_get(_G, _Source, _PreorderTree) :- !.

% data
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.

% support
delete_mst(G) :- retractall(vertex_previous(G, _V, _U)),
    retractall(vertex_key(G, _Vv, _K)), delete_heap(G), !.
delete_mst(_G) :- !.

% Serve il check?
new_vertex_previous(G, V, U) :- vertex_previous(G, V, U), !.
new_vertex_previous(G, V, U) :- assert(vertex_previous(G, V, U)).

new_vertex_key(G, V, K) :- vertex_key(G, V, K), !.
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), KOld = inf, !,
    retract(vertex_key(G, V, KOld)), assert(vertex_key(G, V, K)).
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), KOld =< K, !.
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), K < KOld, !,
    retract(vertex_key(G, V, KOld)), assert(vertex_key(G, V, K)).
new_vertex_key(G, V, K) :- assert(vertex_key(G, V, K)).

heap_add_arcs(G, V) :- vertex_neighbors(G, V, Ns), heap_insert_arcs(G, Ns).
heap_insert_arcs(_G, []) :- !.
heap_insert_arcs(G, [A | Ls]) :- !, A =.. [arc, G, _U, _V, W],
    heap_insert(G, W, A), heap_insert_arcs(G, Ls).