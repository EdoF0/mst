

; mst


;  hashtables
;  now: (graph-id vertex-id) -> T
;  next: non esiste, usa vertex-key
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
;  nuova hashtable mst-id -> mst-id
(defparameter *mst* (make-hash-table :test #'equal :size 10 :rehash-size 1))

;  esecuzione
(defun mst-prim (graph-id source)
  (if (not (null (is-vertex graph-id source)))
         (progn
           (new-mst graph-id)
           (new-vertex-key graph-id source most-positive-double-float)
           (new-vertex-visited graph-id source)
           (heap-add-arcs graph-id source)
           (mst-recursive graph-id)
           nil)))

(defun mst-get (graph-id source)
  (mst-get-floor graph-id source (mst-order-arcs (mst-vertex-neighbors graph-id source))))

;  dati
(defun mst-vertex-key (graph-id vertex-id)
  (or (gethash (list graph-id vertex-id) *vertex-keys*) most-positive-double-float))

(defun mst-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

;  supporto
(defun is-mst (mst-id)
  (if (gethash mst-id *mst*) mst-id))
(defun new-mst (mst-id)
  (if (is-mst mst-id) (delete-mst mst-id))
  (new-heap mst-id (length (graph-arcs mst-id)))
  (hashtable-insert mst-id (cons 0 0) *mst*))
(defun delete-mst (mst-id)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (first key) mst-id))
   *vertex-keys*)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (first key) mst-id))
   *previous*)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (first key) mst-id))
   *visited*)
  (remhash mst-id *mst*))

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

;(defun mst-recursive (graph-id remaning-vertices fails)
;  (write remaning-vertices)
;  (format t "~%")
;  (write fails)
;  (format t "~%")
;  (format t "~%")
;  (let ((arc (second (heap-extract graph-id))))
;    (if (or (null arc) (<= remaning-vertices 0)) remaning-vertices
;      (let ((from (third arc)) (to (fourth arc)) (weight (fifth arc)))
;        (cond
;         ((and (is-visited graph-id from) (is-visited graph-id to))
;          (mst-recursive graph-id remaning-vertices (1+ fails)))
;         ((is-visited graph-id from)
;          (progn
;            (mst-grow graph-id from to weight)
;            (heap-add-arcs graph-id to)
;            (mst-recursive graph-id (1- remaning-vertices) 0)))
;         ((is-visited graph-id to)
;          (progn
;            (mst-grow graph-id to from weight)
;            (heap-add-arcs graph-id from)
;            (mst-recursive graph-id (1- remaning-vertices) 0))))))))
(defun mst-recursive (graph-id)
  (let ((arc (second (heap-extract graph-id))))
    (if (null arc) T
      (let ((from (third arc)) (to (fourth arc)) (weight (fifth arc)))
        (cond
         ((and (is-visited graph-id from) (is-visited graph-id to))
          (mst-recursive graph-id))
         ((is-visited graph-id from)
          (progn (mst-grow graph-id from to weight)
            (heap-add-arcs graph-id to) (mst-recursive graph-id)))
         ((is-visited graph-id to)
          (progn (mst-grow graph-id to from weight)
            (heap-add-arcs graph-id from) (mst-recursive graph-id))))))))

(defun mst-grow (graph-id from to weight)
  (new-vertex-key graph-id to weight)
  (new-vertex-previous graph-id from to)
  (new-vertex-key graph-id from weight)
  (new-vertex-visited graph-id to))

(defun heap-add-arcs (graph-id vertex-id)
  (mapcar
   (lambda (arc)
     (heap-insert graph-id (fifth arc) arc))
   (graph-vertex-neighbors graph-id vertex-id))
  T)

(defun mst-get-floor (graph-id source ordered-arcs)
  (let ((arc (first ordered-arcs)))
    (if (not (null arc))
        (let ((from (third arc)) (to (fourth arc)))
          (if (strn= from source)
              (append
               (list arc)
               (mst-get graph-id to)
               (mst-get-floor graph-id source (rest ordered-arcs)))
            (append 
             (list arc)
             (mst-get graph-id from)
             (mst-get-floor graph-id source (rest ordered-arcs))))))))

(defun mst-vertex-neighbors (graph-id parent)
  (mst-vertex-neighbors-recursive
   graph-id parent
   (hashtable-get-reverse
    (lambda (key val)
      (declare (ignore key))
      (strn= val parent))
    *previous*)))
(defun mst-vertex-neighbors-recursive (graph-id parent vertices)
  (let ((child (second (first vertices))))
    (if (not (null child))
        (cons
         (is-arc-fluid graph-id parent child)
         (mst-vertex-neighbors-recursive graph-id parent (rest vertices))))))

(defun mst-order-arcs (arcs)
  (sort arcs #'strn< :key #'fourth)
  (stable-sort arcs #'< :key #'fifth)
  arcs)

(defun beautify-arc (arc parent)
  (cond ((strn= (third arc) parent) arc)
        (T (list 'arc (second arc) parent (third arc) (fifth arc)))))


; grafi


;  vertex-rep: ('arc graph-id vertex-id vertex-id weight)
;  arc-rep: ('arc graph-id vertex-id vertex-id weight)
; hashtables
;  now: graph-id -> (number-of-vertices . number-of-arcs)
(defparameter *graphs* (make-hash-table :test #'equal :size 10 :rehash-size 1))
;  now: ('vertex graph-id vertex-id) -> ('vertex graph-id vertex-id)
;  next: (graph-id vertex-id) -> (list of couples '(weight . vertex-id))
;  hashtable per ogni grafo?
(defparameter *vertices* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: ('arc graph-id vertexS-id vertexT-id) -> ('arc graph-id vertexS-id vertexT-id weight)
;  next: non esiste
(defparameter *arcs* (make-hash-table :test #'equal :size 100000 :rehash-size 100000))

;  creazione e modifica
(defun new-graph (graph-id)
  (if (is-graph graph-id) (delete-graph graph-id))
  (hashtable-insert graph-id (cons 0 0) *graphs*)
  graph-id)
(defun delete-graph (graph-id)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (second key) graph-id))
   *vertices*)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (second key) graph-id))
   *arcs*)
  (remhash graph-id *graphs*)
  NIL)

(defun new-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
      (if (is-vertex graph-id vertex-id)
          (list 'vertex graph-id vertex-id)
        (progn
          (hashtable-insert graph-id (cons (1+ (graph-vertices-n graph-id)) (graph-arcs-n graph-id)) *graphs*)
          (hashtable-insert (list 'vertex graph-id vertex-id) (list 'vertex graph-id vertex-id) *vertices*)
          (make-vertex-rep graph-id vertex-id)))))

(defun new-arc (graph-id vertexS-id vertexT-id &optional (weight 1))
  (and
   (numberp weight)
   (>= weight 0)
   ;(is-vertex graph-id vertexS-id)
   ;(is-vertex graph-id vertexT-id)
   (new-vertex graph-id vertexS-id)
   (new-vertex graph-id vertexT-id)
   (if (not (is-arc-fluid graph-id vertexS-id vertexT-id))
       (hashtable-insert graph-id (cons (graph-vertices-n graph-id) (1+ (graph-arcs-n graph-id))) *graphs*)
     T)
   (hashtable-insert (list 'arc graph-id vertexS-id vertexT-id) (list 'arc graph-id vertexS-id vertexT-id weight) *arcs*)
   (make-arc-rep graph-id vertexS-id vertexT-id weight)))

;  lettura
(defun is-graph (graph-id)
  (if (gethash graph-id *graphs*) graph-id))

(defun graph-vertices (graph-id)
  (hashtable-get
   (lambda (key val)
     (declare (ignore val))
     (strn= (second key) graph-id))
   *vertices*))
(defun graph-arcs (graph-id)
  (hashtable-get
   (lambda (key val)
     (declare (ignore val))
     (strn= (second key) graph-id))
   *arcs*))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (hashtable-get
   (lambda (key val)
     (declare (ignore val))
     (and
      (strn= (second key) graph-id)
      (or
       (strn= (third key) vertex-id)
       (strn= (fourth key) vertex-id))))
     *arcs*))
(defun graph-vertex-adjacent (graph-id vertex-id)
  (arcs-to-vertices (graph-vertex-neighbors graph-id vertex-id) graph-id vertex-id))

;  stampa
(defun graph-print (graph-id)
  (format t "VERTICI:~%")
  (list-vertices graph-id)
  (format t "~%ARCHI:~%")
  (list-arcs graph-id)
  NIL)

;  supporto
(defun make-vertex-rep (graph-id vertex-id)
  (list 'vertex graph-id vertex-id))
(defun make-arc-rep (graph-id vertexS-id vertexT-id weight)
  (list 'arc graph-id vertexS-id vertexT-id weight))

(defun graph-vertices-n (graph-id)
  (car (gethash graph-id *graphs*)))
(defun graph-arcs-n (graph-id)
  (cdr (gethash graph-id *graphs*)))

(defun is-vertex (graph-id vertex-id)
  (and
   (is-graph graph-id)
   (second (multiple-value-list (gethash (list 'vertex graph-id vertex-id) *vertices*)))))
(defun is-arc (graph-id vertexS-id vertexT-id &optional weight)
  (if (and (is-graph graph-id) (is-vertex graph-id vertexS-id) (is-vertex graph-id vertexT-id))
   (if (null weight)
       (first (multiple-value-list (gethash (list 'arc graph-id vertexS-id vertexT-id) *arcs*)))
     (let ((arc (gethash (list 'arc graph-id vertexS-id vertexT-id) *arcs*)))
      (if (strn= weight (fifth arc)) arc)))))
(defun is-arc-fluid (graph-id vertexS-id vertexT-id &optional weight)
  (if (is-arc graph-id vertexS-id vertexT-id weight)
      (is-arc graph-id vertexS-id vertexT-id weight)
    (is-arc graph-id vertexT-id vertexS-id weight)))
(defun is-arc-single (graph-id vertexS-id vertexT-id &optional weight)
  (if (strn< vertexS-id vertexT-id) (is-arc graph-id vertexS-id vertexT-id weight)))

(defun arcs-to-vertices (arcs graph-id vertex-ignore)
  (if (not (null arcs))
      (let ((arc (first arcs)))
        (cond
         ((and (strn= (second arc) graph-id) (strn= (third arc) vertex-ignore))
          (cons (list 'vertex graph-id (fourth arc)) (arcs-to-vertices (rest arcs) graph-id vertex-ignore)))
         ((and (strn= (second arc) graph-id) (strn= (fourth arc) vertex-ignore))
          (cons (list 'vertex graph-id (third arc)) (arcs-to-vertices (rest arcs) graph-id vertex-ignore)))
         (T (arcs-to-vertices (rest arcs) graph-id vertex-ignore))))))

(defun list-vertices (graph-id)
  (hashtable-get
   (lambda (key val)
     (and (strn= (second key) graph-id)
          (not (format t "~A~%" val))))
   *vertices*))
(defun list-arcs (graph-id)
  (hashtable-get
   (lambda (key val)
     (and (strn= (second key) graph-id)
          (not (format t "~A~%" val))))
   *arcs*))


; minheap


;  heap-rep: ('heap heap-id heap-size actual-heap)
;  hashtables
;  now: heap-id -> ('heap heap-id heap-size actual-heap)
;  next: heap-id -> (heap-size actual-heap)
(defparameter *heaps* (make-hash-table :test #'equal :size 10 :rehash-size 1))

;  creazione e modifica
(defun new-heap (heap-id &optional (capacity 42))
  (hashtable-insert heap-id (list 'heap heap-id 0 (make-array capacity)) *heaps*))
(defun heap-delete (heap-id)
  (remhash heap-id *heaps*) T)

(defun heap-insert (heap-id key val)
  (let ((array (heap-array heap-id))
        (size (heap-index heap-id)))
    (if (and (numberp key) (< size (length array)))
        (progn
          (setf (aref array size) (list key val))
          (heap-increment heap-id)
          (heapify-up array size)))))
(defun heap-extract (heap-id)
  (let ((size (heap-index heap-id))
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
  (zerop (heap-index heap-id)))
(defun heap-not-empty (heap-id)
  (> (heap-index heap-id) 0))

(defun heap-head (heap-id)
  (aref-strong (heap-array heap-id) 0))

(defun heap-id (heap-rep)
  (if (correct-heap-rep heap-rep) (second heap-rep)))
(defun heap-size (heap-rep)
  (if (correct-heap-rep heap-rep) (third heap-rep)))
(defun heap-actual-heap (heap-rep)
  (if (correct-heap-rep heap-rep) (fourth heap-rep)))

;  stampa
(defun heap-print (heap-id)
  (if (is-heap heap-id)
      (not (format t "Lo heap contine ~D elementi:~%~A~%"
                   (heap-index heap-id) (heap-array heap-id)))))

;  supporto
(defun is-heap (heap-id)
  (gethash heap-id *heaps*))
(defun heap-array (heap-id)
  (fourth (gethash heap-id *heaps*)))
(defun heap-index (heap-id)
  (let ((size (third (gethash heap-id *heaps*))))
    (if (numberp size) size 0)))

(defun correct-heap-rep (heap-rep)
  (let ((heap-rep-id (second heap-rep)))
    (and (is-heap heap-rep-id)
         (= (third heap-rep) (heap-index heap-rep-id))
         (equal (fourth heap-rep) (heap-array heap-rep-id)))))

(defun heap-increment (heap-id)
  (if (is-heap heap-id)
      (hashtable-insert heap-id
                        (list 'heap heap-id (1+ (heap-index heap-id)) (heap-array heap-id))
                        *heaps*)))
(defun heap-decrement (heap-id)
  (if (is-heap heap-id)
      (hashtable-insert heap-id
                        (list 'heap heap-id (1- (heap-index heap-id)) (heap-array heap-id))
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
        (index-key (first (aref-strong array index)))
        (left-key (first (aref-strong array (left-child-idx index))))
        (right-key (first (aref-strong array (right-child-idx index))))
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
(defun hashtable-get (condition-function hashtable)
  (let ((out-list ()))
    (maphash
     (lambda (key val)
       (if (funcall condition-function key val) (push val out-list)))
     hashtable)
    out-list))
(defun hashtable-get-reverse (condition-function hashtable)
  (let ((out-list ()))
    (maphash
     (lambda (key val)
       (if (funcall condition-function key val) (push key out-list)))
     hashtable)
    out-list))
(defun hashtable-remove (condition-function hashtable)
  (maphash
   (lambda (key val)
     (if (funcall condition-function key val) (remhash key hashtable)))
   hashtable))

(defun aref-strong (array? index)
  (if (and (arrayp array?)
           (< index (length array?))
           (>= index 0))
      (let ((value (aref array? index)))
        (if (not (strn= value 0)) value))))

(defun swap-entries (array i1 i2)
  (let ((e1 (aref-strong array i1))
        (e2 (aref-strong array i2)))
    (if (< (max i1 i2) (length array))
        (progn
          (if (/= i1 i2) (progn
                           (setf (aref array i1) e2)
                           (setf (aref array i2) e1)))
          array))))