

; mst


;  hashtables
;  now: (graph-id vertex-id) -> T
;  next: same
;  hashtable per ogni grafo?
(defparameter *visited* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: (graph-id vertex-id) -> weight
;  next: same
;  hashtable per ogni grafo?
(defparameter *vertex-keys* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: (graph-id child) -> parent
;  next: same
;  hashtable per ogni grafo?
(defparameter *previous* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))

;  esecuzione
(defun mst-prim (graph-id source)
  (cond ((not (null (vertex-in-graph graph-id source)))
         (progn 
           (delete-mst graph-id)
           (new-heap graph-id (length (graph-arcs graph-id)))
           (new-vertex-key graph-id source most-positive-double-float)
           (new-vertex-visited graph-id source)
           (heap-add-arcs graph-id source)
           (mst-recursive graph-id)
           nil))))

(defun mst-get (graph-id source)
  (mst-get-floor graph-id source
                 (mst-order-arcs
                  (mst-vertex-neighbors graph-id source))))

;  dati
(defun mst-vertex-key (graph-id vertex-id)
  (or (gethash (list graph-id vertex-id) *vertex-keys*) most-positive-double-float))

(defun mst-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

;  supporto
(defun delete-mst (graph-id)
  (maphash (lambda (key val)
             (if (strn= (first key) graph-id) (remhash key *vertex-keys*)))
           *vertex-keys*)
  (maphash (lambda (key val)
             (if (strn= (first key) graph-id) (remhash key *previous*)))
           *previous*)
  (maphash (lambda (key val)
             (if (strn= (first key) graph-id) (remhash key *visited*)))
           *visited*))

(defun mst-recursive (graph-id)
  (let ((head (heap-extract graph-id)))
    (if (null head) t
      (let ((arc (second head)))
        (let ((from (third arc)) (to (fourth arc)) (weight (fifth arc)))
          (cond ((null arc) (write 'casobase))
                ((and (is-visited graph-id from) (is-visited graph-id to))
                 (mst-recursive graph-id))
                ((is-visited graph-id from)
                 (progn (mst-grow graph-id from to weight)
                   (heap-add-arcs graph-id to) (mst-recursive graph-id)))
                ((is-visited graph-id to)
                 (progn (mst-grow graph-id to from weight)
                   (heap-add-arcs graph-id from) (mst-recursive graph-id)))))))))

(defun mst-grow (graph-id from to weight)
  (new-vertex-key graph-id to weight)
  (new-vertex-previous graph-id from to)
  (new-vertex-key graph-id from weight)
  (new-vertex-visited graph-id to))

(defun mst-get-floor (graph-id source ordered-arcs)
  (if (null ordered-arcs) nil
    (let ((from (third (first ordered-arcs))) (to (fourth (first ordered-arcs))))
      (if (strn= from source)
          (append
           (list (first ordered-arcs))
           (mst-get graph-id to)
           (mst-get-floor graph-id source (cdr ordered-arcs)))
        (append 
         (list (first ordered-arcs))
         (mst-get graph-id from)
         (mst-get-floor graph-id source (cdr ordered-arcs)))))))

(defun mst-vertex-neighbors (graph-id parent)
  (let ((children ()) (arcs ()))
    (maphash (lambda (key val)
               (if (strn= val parent) (push (second key) children)))
             *previous*)
    (mapcar (lambda (child)
              (push (beautify-arc
                     (or (gethash (list 'arc graph-id parent child) *arcs*)
                         (gethash (list 'arc graph-id child parent) *arcs*))
                     parent) arcs))
            children)
    arcs))

(defun mst-order-arcs (arcs)
  (sort arcs #'strn< :key #'fourth)
  (stable-sort arcs #'< :key #'fifth)
  arcs)

(defun is-visited (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *visited*))

(defun new-vertex-key (graph-id vertex-id weight)
  (let ((old-weight (mst-vertex-key graph-id vertex-id)))
    (cond ((> old-weight weight)
           (hashtable-insert (list graph-id vertex-id) weight *vertex-keys*))) T))

(defun new-vertex-visited (graph-id vertex-id)
  (hashtable-insert (list graph-id vertex-id) T *visited*))

(defun new-vertex-previous (graph-id parent child)
  (hashtable-insert (list graph-id child) parent *previous*))

(defun heap-add-arcs (graph-id vertex-id)
  (mapcar (lambda (arc) (heap-insert graph-id (fifth arc) arc))
          (graph-vertex-neighbors graph-id vertex-id))
  T)


; grafi


; hashtables
;  now: graph-id -> graph-id
;  next: graph-id -> (number-of-vertices number-of-arcs)
(defparameter *graphs* (make-hash-table :test #'equal :size 10 :rehash-size 1))
;  now: ('vertex graph-id vertex-id) -> ('vertex graph-id vertex-id)
;  next: (graph-id vertex-id) -> T
;  hashtable per ogni grafo?
(defparameter *vertices* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: ('arc graph-id vertexS-id vertexT-id) -> ('arc graph-id vertexS-id vertexT-id weight)
;  next: (graph-id vertexS-id vertexT-id) -> weight
;  hashtable per ogni grafo?
(defparameter *arcs* (make-hash-table :test #'equal :size 100000 :rehash-size 100000))

;  creazione e modifica
(defun new-graph (graph-id)
  (delete-graph graph-id)
  (hashtable-insert graph-id graph-id *graphs*))

(defun delete-graph (graph-id)
  (maphash (lambda (key val) (if (strn= (second key) graph-id)
                                 (remhash val *vertices*)))
           *vertices*)
  (maphash (lambda (key val) (if (strn= (second key) graph-id)
                                 (remhash val *arcs*)))
           *arcs*)
  (remhash graph-id *graphs* )
  nil)

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-vertex (graph-id vertex-id)
  (and 
   (is-graph graph-id)
   (hashtable-insert (list 'vertex graph-id vertex-id) (list 'vertex graph-id vertex-id) *vertices*)))

(defun new-arc (graph-id vertexS-id vertexT-id &optional (weight 1))
  (and 
   (vertex-in-graph graph-id vertexS-id)
   (vertex-in-graph graph-id vertexT-id)
   (hashtable-insert (list 'arc graph-id vertexS-id vertexT-id) (list 'arc graph-id vertexS-id vertexT-id weight) *arcs*)))

;  lettura
(defun graph-vertices (graph-id)
  (let ((vertices  ()))
    (maphash (lambda (key val)
               (if (strn= (second key) graph-id)
                   (push val vertices)))
             *vertices*)
    vertices))

(defun graph-arcs (graph-id)
  (let ((arcs ()))
    (maphash (lambda (key val)
               (if (strn= (second key) graph-id)
                   (push val arcs)))
             *arcs*)
    arcs))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((arcs ()))
    (maphash (lambda (key val)
               (if
                   (and (strn= (second key) graph-id)
                        (or
                         (strn= (third key) vertex-id)
                         (strn= (fourth key) vertex-id)))
                   (push val arcs)))
             *arcs*)
    arcs))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (let ((arcs ()) (vertices ()))
    (maphash (lambda (key val)
               (if (and (strn= (second key) graph-id) 
                        (or
                         (strn= (third key) vertex-id) 
                         (strn= (fourth key) vertex-id)))
                   (push val arcs)))
             *arcs*)
    (mapcar (lambda (arc)
              (if (equal (third arc) vertex-id)
                  (push (list 'vertex graph-id (fourth arc)) vertices)
                (push (list 'vertex graph-id (third arc)) vertices)))
            arcs)
    vertices))

;  stampa
(defun graph-print (graph-id)
  (format t "VERTICI:~%")
  (list-vertices graph-id)
  (format t "~%ARCHI:~%")
  (list-arcs graph-id))

;  supporto
(defun beautify-arc (arc parent)
  (cond ((strn= (third arc) parent) arc)
        (T (list 'arc (second arc) parent (third arc) (fifth arc)))))

(defun list-vertices (graph-id)
  (maphash (lambda (key val)
             (if (strn= (second key) graph-id)
                 (format t "~A~%" val)))
           *vertices*))

(defun list-arcs (graph-id)
  (maphash (lambda (key val)
             (if (strn= (second key) graph-id)
                 (format t "~A~%" val)))
           *arcs*))

(defun vertex-in-graph (graph-id vertex-id)
  (and
   (is-graph graph-id)
   (gethash (list 'vertex graph-id vertex-id) *vertices*)))


; minheap


;  hashtables
;  now: heap-id -> ('heap heap-id last-element array))
;  next: same but without 'heap ?
(defparameter *heaps* (make-hash-table :test #'equal :size 10 :rehash-size 1))

;  creazione e modifica
(defun new-heap (heap-id &optional (capacity 42))
  (hashtable-insert heap-id (list 'heap heap-id 0 (make-array capacity)) *heaps*))
(defun heap-delete (heap-id)
  (remhash heap-id *heaps*) T)

(defun heap-insert (heap-id key val)
  (let ((array (heap-array heap-id))
        (size (heap-size heap-id)))
    (if (and (numberp key) (< size (length array)))
        (progn
          (setf (aref array size) (list key val))
          (heap-increment heap-id)
          (heapify-up array size)))))
(defun heap-extract (heap-id)
  (let ((size (heap-size heap-id))
        (array (heap-array heap-id))
        (head (heap-head heap-id)))
    (if (and (> size 0)
             (heapify (swap-entries array 0 (1- size)) 0 (- size 2)))
        (progn
          (setf (aref array (1- size)) NIL)
          (heap-decrement heap-id)
          head))))

;  lettura
(defun heap-empty (heap-id)
  (zerop (heap-size heap-id)))
(defun heap-not-empty (heap-id)
  (> (heap-size heap-id) 0))

(defun heap-head (heap-id)
  (aref-strong (heap-array heap-id) 0))

;  stampa
(defun heap-print (heap-id)
  (if (is-heap heap-id)
      (progn
        (format t "Lo heap contine ~D elementi:~%~A~%"
                (heap-size heap-id) (heap-array heap-id))
        T)))

;  supporto
(defun is-heap (heap-id)
  (gethash heap-id *heaps*))
(defun heap-array (heap-id)
  (fourth (gethash heap-id *heaps*)))
(defun heap-size (heap-id)
  (let ((size (third (gethash heap-id *heaps*))))
    (if (numberp size) size 0)))

(defun heap-increment (heap-id)
  (if (is-heap heap-id)
      (hashtable-insert heap-id
                        (list 'heap heap-id (1+ (heap-size heap-id)) (heap-array heap-id))
                        *heaps*)))
(defun heap-decrement (heap-id)
  (if (is-heap heap-id)
      (hashtable-insert heap-id
                        (list 'heap heap-id (1- (heap-size heap-id)) (heap-array heap-id))
                        *heaps*)))

(defun parent-idx (index)
  (if (<= index 2) 0
    (first (multiple-value-list (floor (/ (1- index) 2))))))
(defun left-child-idx (index)
  (1- (* (1+ index) 2)))
(defun right-child-idx (index)
  (* (1+ index) 2))

(defun heapify (array index &optional (max most-positive-fixnum))
  (let ((left-index (left-child-idx index))
        (right-index (right-child-idx index))
        (index-key (car (aref-strong array index)))
        (left-key (car (aref-strong array (left-child-idx index))))
        (right-key (car (aref-strong array (right-child-idx index))))
        (max-index (min max (1- (length array)))))
    (cond
     ((> left-index max-index) T)
     ((> right-index max-index)
      (if (< left-key index-key)
          (heapify (swap-entries array index left-index) left-index max-index)
        T))
     ((= (min index-key left-key right-key) index-key) T)
     ((= (min index-key left-key right-key) left-key)
      (heapify (swap-entries array index left-index) left-index max-index))
     ((= (min index-key left-key right-key) right-key)
      (heapify (swap-entries array index right-index) right-index max-index)))))

(defun heapify-up (array index)
  (let ((parent-index (parent-idx index))
        (index-key (car (aref-strong array index)))
        (parent-key (car (aref-strong array (parent-idx index)))))
    (cond
      ((<= index 0) T)
      ((<= parent-key index-key) T)
      ((< index-key parent-key)
       (heapify-up (swap-entries array index parent-index) parent-index)))))


; supporto generico


(defun strn= (val1 val2)
  (string= (write-to-string val1) (write-to-string val2)))
(defun strn< (val1 val2)
  (string< (write-to-string val1) (write-to-string val2)))

(defun hashtable-insert (key value hashtable)
  (setf (gethash key hashtable) value))

(defun aref-strong (array? index)
  (if (and (arrayp array?)
           (< index (length array?))
           (>= index 0))
      (aref array? index)))

(defun swap-entries (array i1 i2)
  (let ((e1 (aref-strong array i1))
        (e2 (aref-strong array i2)))
    (if (< (max i1 i2) (length array))
        (progn
          (if (/= i1 i2) (progn
                           (setf (aref array i1) e2)
                           (setf (aref array i2) e1)))
          array))))