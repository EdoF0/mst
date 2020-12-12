

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
new_arc(G, U, V, W) :- number(W), W > 0,
    graph(G), vertex(G, V),
    vertex(G, U), check_arc(G, U, V, W2),
    delete_arc(G, U, V, W2),
    assert(arc(G, U, V, W)), !.
new_arc(G, U, V, W) :- number(W), W > 0,
    graph(G), vertex(G, V),
    vertex(G, U), assert(arc(G, U, V, W)).
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% reading
graph_vertices(G, Vs) :- findall(vertex(G, V),
    vertex(G, V), Vs).

graph_arcs(G, Es) :- findall(arc(G, U, V, W),
    arc(G, U, V, W), Es).

vertex_neighbors(G, V, Ns) :- vertex(G, V),
    findall(arc(G, V, N, W),
    check_arc(G, V, N, W), Ns).

adjs(G, V, Vs) :- findall(vertex(G, U),
    check_arc(G, V, U, _), Vs).

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