

% graph


% creation and edit
% vertex(graph, vertex, [3-a, 1-b, ...]),
% vertex(graph, a, [3-vertex])

new_graph(G) :- nonvar(G), graph(G), !.
new_graph(G) :- nonvar(G), assert_graph(G).

delete_graph(G) :- retract_graph(G).

new_vertex(G, V) :- maplist(nonvar, [G,V]), vertex(G, V), !.
new_vertex(G, V) :- maplist(nonvar, [G,V]), assert_vertex(G, V), !.

new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), arc(G, U, V, W), !.
new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), arc(G, U, V, _), !,
    update_arc(G, U, V, W).
new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), assert_arc(G, U, V, W), !.
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% reading
graph_vertices(G, Vs) :- findall(vertex(G, V), vertex(G, V), Vs).
graph_arcs(G, Es) :- findall(arc(G, U, V, W), arc_single_fluid(G, U, V, W), Es).

vertex_neighbors(G, V, Ns) :- findall(arc(G, V, U, W), arc_fluid(G, V, U, W), Ns).
adjs(G, V, Vs) :- findall(vertex(G, U), arc_fluid(G, V, U, _), Vs).

% print
list_vertices(G) :- graph_vertices(G, Vs), printlist(Vs).
list_arcs(G) :- graph_arcs(G, As), printlist(As).
list_graph(G) :- list_vertices(G), nl, list_arcs(G).

% csv file
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows,
        [separator(0'\t),
        functor(ga), arity(3)]),
    new_graph(G), add_from_ga(G, Rows).

    % write graph solo di archi singoli?
write_graph(G, FileName, graph) :- graph_arcs(G, As),
    write_graph(As, FileName, edges), !.
write_graph(As, FileName, edges) :-
    arcs_to_gas(As, Gas),
    csv_write_file(FileName, Gas,
        [separator(0'\t),
        functor(ga), arity(3)]), !.
write_graph(G, FileName) :-
    write_graph(G, FileName, graph).

% support
delete_first([El | L1], El, L1) :- !.
delete_first([E | L1], El, [E | L2]) :- delete_first(L1, El, L2).
member_first(El, [El | _]) :- !.
member_first(El, [_ | L]) :- member_first(El, L).

:- dynamic graph/1.
assert_graph(G) :- not(graph(G)), assert(graph(G)).
retract_graph(G) :- graph(G), retract(graph(G)), retractall(vertex(G,_,_)).

vertex(G, V) :- vertex(G, V, _).
:- dynamic vertex/3.
assert_vertex(G, V) :- assert_vertex(G, V, []).
assert_vertex(G, V, A) :- not(vertex(G, V)), assert(vertex(G, V, A)).
    % retract_vertex(G, V). retract vertex
update_vertex(G, V, ANew) :- vertex(G, V, A),
    retract(vertex(G, V, A)), assert(vertex(G, V, ANew)).

arc(G, U, V, W) :- vertex(G, U, A), member_first(W-V, A).
arc_fluid(G, U, V, W) :- vertex(G, U, A), member(W-V, A).
arc_single_fluid(G, U, V, W) :- vertex(G, U, A), member(W-V, A), U @< V.
assert_arc(G, U, V, W) :- not(arc(G, U, V, W)), not(arc(G, V, U, W)),
    vertex(G, U, AU), vertex(G, V, AV),
    update_vertex(G, U, [ W-V | AU]), update_vertex(G, V, [ W-U | AV]).
retract_arc(G, U, V, W) :- arc(G, U, V, W), arc(G, V, U, W),
    vertex(G, U, AU), vertex(G, V, AV),
    delete_first(AU, W-V, AUNew), delete_first(AV, W-U, AVNew),
    update_vertex(G, U, AUNew), update_vertex(G, V, AVNew).
update_arc(G, U, V, WNew) :- arc(G, U, V, W), arc(G, V, U, W),
    vertex(G, U, AU), vertex(G, V, AV),
    delete_first(AU, W-V, AUNew), delete_first(AV, W-U, AVNew),
    update_vertex(G, U, [ WNew-V | AUNew]), update_vertex(G, V, [ WNew-U | AVNew]).
sort_vertex(G, V) :- vertex(G, V, A),
    sort(2, @=<, A, ANew), sort(1, =<, ANew, ANew2),
    update_vertex(G, V, ANew2).

printlist([]) :- !.
printlist([E | L]) :-
    write(E), nl,
    printlist(L).

% generic_arcs ga(s)
add_from_ga(_, []) :- !.
add_from_ga(G, [ga(V, U, W) | Gas]) :-
    new_vertex(G, V), new_vertex(G, U),
    new_arc(G, V, U, W), add_from_ga(G, Gas).

arcs_to_gas([], []).
arcs_to_gas([arc(_, V, U, W) | As], [ga(V, U, W) | Gas]) :-
    arcs_to_gas(As, Gas).


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
mst_prim(G, Source) :- delete_mst(G), graph(G), new_heap(G),
    vertex(G, Source), new_vertex_key(G, Source, inf), heap_add_arcs(G, Source),
    mst_prim(G), mst_inf(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), vertex_key(G, V, _), !, heap_extract(G, W, A), mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), !, mst_grow(G, U, V, W),
    heap_extract(G, W, A), heap_add_arcs(G, V), mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, V, _), !, mst_grow(G, V, U, W),
    heap_extract(G, W, A), heap_add_arcs(G, U), mst_prim(G).
mst_prim(G) :- heap_empty(G), !.

mst_get(G, Source, []) :- mst_vertex_neighbors(G, Source, []), !.
mst_get(G, Source, PreorderTree) :-
    mst_vertex_neighbors(G, Source, Arcs),
    mst_order_arcs(Arcs, OArcs),
    mst_get(G, Source, OArcs, PreorderTree).
mst_get(G, Source, [ Arc | Arcs], [Arc | PreorderTree]) :-
    Arc =.. [arc, G, Source, V, _W], !,
    mst_get(G, V, PreorderTreeChild),
    mst_get(G, Source, Arcs, PreorderTreeRest),
    append(PreorderTreeChild, PreorderTreeRest, PreorderTree).
mst_get(G, Source, [ Arc | Arcs], [Arc | PreorderTree]) :-
    Arc =.. [arc, G, U, Source, _W], !,
    mst_get(G, U, PreorderTreeChild),
    mst_get(G, Source, Arcs, PreorderTreeRest),
    append(PreorderTreeChild, PreorderTreeRest, PreorderTree).
mst_get(_G, _Source, [], []) :- !.

% data
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.

% support
delete_mst(G) :- retractall(vertex_previous(G, _V, _U)),
    retractall(vertex_key(G, _Vv, _K)), delete_heap(G), !.
delete_mst(_G) :- !.

mst_grow(G, U, V, W) :-
    new_vertex_key(G, U, W), new_vertex_previous(G, V, U), new_vertex_key(G, U, W).

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

mst_inf(G) :- findall(G-V, mst_excluded(G, V), L), mst_set_inf(L).
mst_excluded(G, V) :- vertex(G, V), not(vertex_key(G, V, _)).
mst_set_inf([]) :- !.
mst_set_inf([G-V | L]) :- assert(vertex_key(G, V, inf)), mst_set_inf(L).

mst_order_arcs(L, Ss) :- sort(3, @=<, L, S), sort(4, =<, S, Ss).

mst_vertex_neighbors(G, Source, Arcs) :-
    findall(A, prev_to_arc(G, Source, A), Arcs).
prev_to_arc(G, S, arc(G, S, U, W)) :- vertex_previous(G, U, S), arc_fluid(G, S, U, W).
