

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

swap_heap_entries(H, P1, P2) :-
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    assert(heap_entry(H, P1, K2, V2)).

buildheap(H) :- heap(H, S), Sn is floor(S/2), buildheap(H, Sn).
buildheap(_H, 0) :- !.
buildheap(H, S) :- heapify(H, S), Sn is S-1, buildheap(H, Sn).

heapify_up(_H, P) :- P < 1, !.
heapify_up(H, P) :- heap(H, _), P = 1, !.
heapify_up(H, P) :- heap(H, _), heap_entry(H, P, K, _),
    Pp is floor(P/2), heap_entry(H, Pp, Kp, _),
    Kp =< K, !.
heapify_up(H, P) :- heap(H, _), heap_entry(H, P, K, _),
    Pp is floor(P/2), heap_entry(H, Pp, Kp, _),
    K < Kp, !,
    swap_heap_entries(H, P, Pp), heapify_up(H, Pp).

heapify(H, P) :- heap(H, S), P > S, !.
heapify(H, P) :- heap(H, S), P*2 > S, !.
heapify(H, P) :- heap(H, S), P*2+1 > S, heap_entry(H, P, K, _),
    Pl is P*2, heap_entry(H, Pl, Kl, _),
    K =< Kl, !.
heapify(H, P) :- heap(H, S), P*2+1 > S, heap_entry(H, P, K, _),
    Pl is P*2, heap_entry(H, Pl, Kl, _),
    Kl < K, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap(H, _), heap_entry(H, P, K, _),
    Pl is P*2, heap_entry(H, Pl, Kl, _),
    Pr is Pl+1, heap_entry(H, Pr, Kr, _),
    K =< Kl, K =< Kr, !.
heapify(H, P) :- heap(H, _), heap_entry(H, P, K, _),
    Pl is P*2, heap_entry(H, Pl, Kl, _),
    Pr is Pl+1, heap_entry(H, Pr, Kr, _),
    Kl < K, Kl < Kr, !,
    swap_heap_entries(H, P, Pl), heapify(H, Pl).
heapify(H, P) :- heap(H, _), heap_entry(H, P, K, _),
    Pl is P*2, heap_entry(H, Pl, Kl, _),
    Pr is Pl+1, heap_entry(H, Pr, Kr, _),
    Kr < K, Kr < Kl, !,
    swap_heap_entries(H, P, Pr), heapify(H, Pr).
