

% graph


% creation and edit
new_graph(G) :- nonvar(G), graph(G), !, delete_graph(G), assert_graph(G).
new_graph(G) :- nonvar(G), assert_graph(G).

delete_graph(G) :- retract_graph(G).

new_vertex(G, V) :- maplist(nonvar, [G,V]), vertex(G, V), !.
new_vertex(G, V) :- maplist(nonvar, [G,V]), assert_vertex(G, V), !, graph_vertex_add(G).

new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), arc(G, U, V, W), !.
new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), arc(G, U, V, _), !,
    update_arc(G, U, V, W).
new_arc(G, U, V, W) :- maplist(nonvar, [G,U,V,W]), assert_arc(G, U, V, W), !, graph_arc_add(G).
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% reading
graph_vertices(G, Vs) :- findall(vertex(G, V), vertex(G, V), Vs).
graph_arcs(G, As) :- findall(arc(G, U, V, W), arc_single_fluid(G, U, V, W), As).

vertex_neighbors(G, V, As) :- findall(arc(G, V, U, W), arc_fluid(G, V, U, W), As).
adjs(G, V, Vs) :- findall(vertex(G, U), arc_fluid(G, V, U, _), Vs).

% print
list_vertices(G) :- graph(G), graph_vertices(G, Vs), printlist(Vs).
list_arcs(G) :- graph(G), graph_arcs(G, As), printlist(As).
list_graph(G) :- graph(G), list_vertices(G), nl, list_arcs(G).

% csv file
read_graph(G, FileName) :-
    csv_read_file(FileName, Rows,
        [separator(0'\t),
        functor(ga), arity(3)]),
    new_graph(G), add_from_ga(G, Rows).

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
%  graph
:- dynamic graph/3.
graph(Graph) :- graph(Graph, _Number_of_Vertices, _Number_of_Arcs).

assert_graph(G) :- assert_graph(G, 0, 0).
assert_graph(G, NV, NA) :- \+ graph(G), assert(graph(G, NV, NA)).
retract_graph(G) :- retract_graph(G, _, _).
retract_graph(G, NV, NA) :- graph(G), retract(graph(G, NV, NA)), retractall(vertex(G,_,_)).
update_graph(G, NVNew, NANew) :- graph(G, NV, NA),
    retract(graph(G, NV, NA)), assert(graph(G, NVNew, NANew)).
update_graph(G) :- graph(G),
    graph_vertices(G, Vs), length(Vs, NV),
    graph_arcs(G, As), length(As, NA),
    update_graph(G, NV, NA).

graph_vertex_add(G) :- graph(G, NV, NA), NVNew is NV+1, update_graph(G, NVNew, NA).
graph_arc_add(G) :- graph(G, NV, NA), NANew is NA+1, update_graph(G, NV, NANew).
graph_vertices_n(G, N) :- graph(G, N, _).
graph_arcs_n(G, N) :- graph(G, _, N).

%  vertex
:- dynamic vertex/3.
vertex(Graph, Vertex) :- vertex(Graph, Vertex, _List_of_couples_Weight-Vertex).

assert_vertex(G, V) :- assert_vertex(G, V, []).
assert_vertex(G, V, A) :- \+ vertex(G, V), assert(vertex(G, V, A)).
%  retract_vertex(G, V). retract vertex + arcs including that vertex
update_vertex(G, V, ANew) :- vertex(G, V, A),
    retract(vertex(G, V, A)), assert(vertex(G, V, ANew)).

%  arc
arc(G, U, V, W) :- vertex(G, U, A), member_first(W-V, A).
arc_fluid(G, U, V, W) :- vertex(G, U, A), member(W-V, A).
arc_single_fluid(G, U, V, W) :- vertex(G, U, A), member(W-V, A), U @< V.

assert_arc(G, U, V, W) :- \+ arc(G, U, V, W), \+ arc(G, V, U, W),
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

%  generic_arcs ga(s)
add_from_ga(_, []) :- !.
add_from_ga(G, [ga(V, U, W) | Gas]) :-
    new_vertex(G, V), new_vertex(G, U),
    new_arc(G, V, U, W), add_from_ga(G, Gas).

arcs_to_gas([], []).
arcs_to_gas([arc(_, V, U, W) | As], [ga(V, U, W) | Gas]) :-
    arcs_to_gas(As, Gas).


% minheap


% creation and edit
new_heap(H) :- nonvar(H), heap(H), !, delete_heap(H), assert_heap(H).
new_heap(H) :- nonvar(H), assert_heap(H), !.

delete_heap(H) :- retract_heap(H).

heap_insert(H, K, V) :- heap(H, S), S > 0, !, integer(K),
    P is S+1, assert_heap_entry(H, P, K, V),
    heap_increment(H), heapify_up(H, P).
heap_insert(H, K, V) :- heap(H, 0), !, integer(K),
    assert_heap_entry(H, 1, K, V),
    heap_increment(H).

heap_extract(H, K, V) :- heap(H, S), var(K), var(V), !,
    heap_head(H, K, V),
    retract_heap_entry(H, 1, K, V), retract_heap_entry(H, S, K1, V1),
    assert_heap_entry(H, 1, K1, V1),
    heap_decrement(H), heapify(H, 1).
heap_extract(H, K, V) :- heap(H, S), heap_entry(H, P, K, V),
    P < S, !,
    retract_heap_entry(H, P, K, V), retract_heap_entry(H, S, K1, V1),
    assert_heap_entry(H, P, K1, V1),
    heap_decrement(H), heapify(H, P), heapify_up(H, P).
heap_extract(H, K, V) :- heap(H, S), heap_entry(H, P, K, V),
    P = S, !,
    retract_heap_entry(H, P, K, V),
    heap_decrement(H).

modify_key(H, NewKey, OldKey, V) :- heap(H),
    retract_heap_entry(H, P, OldKey, V), assert_heap_entry(H, P, NewKey, V),
    heapify(H, P), heapify_up(H, P).

% reading
heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap(H, S), S = 0.
heap_not_empty(H) :- heap(H, S), S > 0.

heap_head(H, K, V) :- heap_entry(H, 1, K, V).

% print
list_heap(H) :- heap(H), listing(heap_entry(H, _P, _K, _V)).

% support
%  heap
:- dynamic heap/2.
heap(Heap) :- heap(Heap, _Size).

assert_heap(H) :- \+ heap(H), assert_heap(H, 0).
assert_heap(H, S) :- \+ heap(H), S >= 0, assert(heap(H, S)).
retract_heap(H) :- heap(H), retract(heap(H, _S)), retractall(heap_entry(H, _P, _K, _V)).
update_heap(H, SNew) :- heap(H, S), retract(heap(H, S)), assert_heap(H, SNew).

heap_increment(H) :- heap(H, S), SNew is S+1, update_heap(H, SNew).
heap_decrement(H) :- heap(H, S), SNew is S-1, update_heap(H, SNew).

%  heap entry
:- dynamic heap_entry/4.
heap_entry(H, P, K) :- heap_entry(H, P, K, _V).

assert_heap_entry(H, P, K, V) :- \+ heap_entry(H, P, _, _), assert(heap_entry(H, P, K, V)).
retract_heap_entry(H, P, K, V) :- retract(heap_entry(H, P, K, V)).

heap_entry_left(H, P, Pl) :- heap(H, S), P >= 1, P =< S, Pl is P*2, Pl =< S.
heap_entry_right(H, P, Pr) :- heap(H, S), P >= 1, P =< S, Pr is P*2+1, Pr =< S.
heap_entry_parent(H, P, Pp) :- heap(H, S), P >= 1, P =< S, Pp is floor(P/2), Pp >= 1.

%  heapify
swap_heap_entries(_H, P, P) :- !.
swap_heap_entries(H, P1, P2) :-
    retract_heap_entry(H, P1, K1, V1),
    retract_heap_entry(H, P2, K2, V2),
    assert_heap_entry(H, P2, K1, V1),
    assert_heap_entry(H, P1, K2, V2).

heapify_up(H, P) :- heap_entry_parent(H, P, Pp),
    heap_entry(H, P, K), heap_entry(H, Pp, Kp),
    Kp =< K, !.
heapify_up(H, P) :- heap_entry_parent(H, P, Pp),
    heap_entry(H, P, K), heap_entry(H, Pp, Kp),
    K < Kp, !,
    swap_heap_entries(H, P, Pp), heapify_up(H, Pp).
heapify_up(H, P) :- heap(H), P =< 1, !.

heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K), heap_entry(H, Pl, Kl), heap_entry(H, Pr, Kr),
    K =< Kl, K =< Kr, !.
heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K), heap_entry(H, Pl, Kl), heap_entry(H, Pr, Kr),
    Kl =< K, Kl =< Kr, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap_entry_right(H, P, Pr), heap_entry_left(H, P, Pl),
    heap_entry(H, P, K), heap_entry(H, Pl, Kl), heap_entry(H, Pr, Kr),
    Kr < K, Kr < Kl, !,
    swap_heap_entries(H, P, Pr), heapify(H, Pr).
heapify(H, P) :- heap_entry_left(H, P, Pl),
    heap_entry(H, P, K), heap_entry(H, Pl, Kl),
    K =< Kl, !.
heapify(H, P) :- heap_entry_left(H, P, Pl),
    heap_entry(H, P, K), heap_entry(H, Pl, Kl),
    Kl < K, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap(H, S), Not_leaves is floor(S/2)+1, P >= Not_leaves, !.

buildheap(H) :- heap(H, S), Sn is floor(S/2), buildheap(H, Sn).
buildheap(_H, 0) :- !.
buildheap(H, S) :- heapify(H, S), Sn is S-1, buildheap(H, Sn).


% mst


% execution
mst_prim(G, Source) :- new_mst(G), vertex(G, Source),
    new_vertex_key(G, Source, inf), heap_add_arcs(G, Source), mst_increment(G),
    mst_prim(G), mst_inf(G).
mst_prim(G) :- mst(G, 0), !.
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), vertex_key(G, V, _), !,
    heap_extract(G, W, A),
    increment_prim_fail_n, mst_clean_heap_cond(G),
    mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, U, _), !, mst_grow(G, U, V, W),
    heap_extract(G, W, A), heap_add_arcs(G, V),
    reset_prim_fail_n, mst_prim(G).
mst_prim(G) :- heap_head(G, W, A), A =.. [arc, G, U, V, W],
    vertex_key(G, V, _), !, mst_grow(G, V, U, W),
    heap_extract(G, W, A), heap_add_arcs(G, U),
    reset_prim_fail_n, mst_prim(G).
mst_prim(G) :- heap_empty(G).

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
vertex_key(M, V) :- vertex_key(M, V, _K).
:- dynamic vertex_previous/3.

% support
%  mst
:- dynamic mst/2.
mst(M) :- mst(M, _Graph_size_minus_mst_size).

new_mst(M) :- graph(M), mst(M), !, delete_mst(M), assert_mst(M), new_heap(M).
new_mst(M) :- graph(M), assert_mst(M), new_heap(M).
delete_mst(M) :- delete_heap(M), !, retract_mst(M).
delete_mst(M) :- retract_mst(M).

assert_mst(M) :- graph_vertices(M, L), length(L, S), assert_mst(M, S).
assert_mst(M, S) :- \+ mst(M), assert(mst(M, S)).
retract_mst(M) :- mst(M), retract(mst(M, _)),
    retractall(vertex_previous(M, _V, _U)), retractall(vertex_key(M, _V2, _K)).
update_mst(M, SNew) :- retract(mst(M, _)), assert_mst(M, SNew).

mst_increment(M) :- mst(M, S), SNew is S-1, update_mst(M, SNew).

%  vertex previous
new_vertex_previous(G, V, Prev) :- assert(vertex_previous(G, V, Prev)).

%  vertex key
new_vertex_key(G, V, K) :- vertex_key(G, V, K), !. % inutile, previsto dal caso 3
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), KOld = inf, !,
    retract(vertex_key(G, V, KOld)), assert(vertex_key(G, V, K)).
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), KOld =< K, !.
new_vertex_key(G, V, K) :- vertex_key(G, V, KOld), K < KOld, !,
    retract(vertex_key(G, V, KOld)), assert(vertex_key(G, V, K)).
new_vertex_key(G, V, K) :- assert(vertex_key(G, V, K)).

%  mst prim support
mst_grow(G, U, V, W) :-
    new_vertex_key(G, V, W), new_vertex_previous(G, V, U), new_vertex_key(G, U, W),
    mst_increment(G).

heap_add_arcs(G, V) :- vertex_neighbors(G, V, Ns), heap_insert_arcs(G, Ns).
heap_insert_arcs(_G, []) :- !.
heap_insert_arcs(G, [A | Ls]) :- A =.. [arc, G, _U, V, _W],
    vertex_key(G, V, _), !,
    heap_insert_arcs(G, Ls).
heap_insert_arcs(G, [A | Ls]) :- A =.. [arc, G, _U, _V, W],
    heap_insert(G, W, A), heap_insert_arcs(G, Ls).

%   clean
:- dynamic prim_fail_n/1.
increment_prim_fail_n :- prim_fail_n(X), !, XNew is X+1, update_prim_fail_n(XNew).
increment_prim_fail_n :- update_prim_fail_n(1).
reset_prim_fail_n :- prim_fail_n(0).
reset_prim_fail_n :- update_prim_fail_n(0).
update_prim_fail_n(X) :- retract(prim_fail_n(_)), !, assert(prim_fail_n(X)).
update_prim_fail_n(X) :- assert(prim_fail_n(X)).

mst_clean_heap_cond(G) :- prim_fail_n(X), X>6, !, mst_clean_heap(G).
mst_clean_heap_cond(_G).
mst_clean_heap(G) :- heap(G, S), mst_clean_heap(G, S), buildheap(G).
mst_clean_heap(_G, 0) :- !.
mst_clean_heap(G, N) :- mst_clean_heap_entry(G, N, _K, _V), !,
    NNew is N-1, mst_clean_heap(G, NNew).
mst_clean_heap(G, N) :- NNew is N-1, mst_clean_heap(G, NNew).
mst_clean_heap_entry(G, P, K, V) :-
    heap(G, S), heap_entry(G, P, K, V), V =.. [arc, G, U, T, _W],
    vertex_key(G, U), vertex_key(G, T),
    swap_heap_entries(G, S, P), retract_heap_entry(G, S, K, V), heap_decrement(G).

%   set non visited vertices (inf)
mst_inf(G) :- findall(G-V, mst_excluded(G, V), L), mst_set_inf(L).
mst_excluded(G, V) :- vertex(G, V), \+ vertex_key(G, V, _).
mst_set_inf([]) :- !.
mst_set_inf([G-V | L]) :- assert(vertex_key(G, V, inf)), mst_set_inf(L).

%  mst get support
mst_order_arcs(L, Ss) :- sort(3, @=<, L, S), sort(4, =<, S, Ss).

mst_vertex_neighbors(G, Source, Arcs) :-
    findall(A, prev_to_arc(G, Source, A), Arcs).
prev_to_arc(G, S, arc(G, S, U, W)) :- vertex_previous(G, U, S), arc_fluid(G, S, U, W).


% general support
delete_first([El | L1], El, L1) :- !.
delete_first([E | L1], El, [E | L2]) :- delete_first(L1, El, L2).
member_first(El, [El | _]) :- !.
member_first(El, [_ | L]) :- member_first(El, L).

printlist([]) :- !.
printlist([E | L]) :-
    write(E), nl,
    printlist(L).
