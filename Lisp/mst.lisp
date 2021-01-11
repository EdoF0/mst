

; mst


;  hashtables
;  now: (graph-id vertex-id) -> weight
(defparameter *vertex-keys* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: (graph-id vertex-child-id) -> vertex-parent-id
(defparameter *previous* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))
;  now: mst-id -> mst-id
(defparameter *mst* (make-hash-table :test #'equal :size 10 :rehash-size 1))

;  esecuzione
(defun mst-prim (graph-id source)
  (if (not (null (is-vertex graph-id source)))
         (progn
           (new-mst graph-id)
           (hashtable-insert (list graph-id source) most-positive-double-float *vertex-keys*)
           (heap-add-arcs graph-id source)
           (mst-recursive graph-id (1- (graph-vertices-n graph-id)) 0)
           nil)))

(defun mst-get (graph-id source)
  (mst-get-floor graph-id source (mst-order-arcs (mst-vertex-neighbors graph-id source))))

;  dati
(defun mst-vertex-key (graph-id vertex-id)
  (or (first (multiple-value-list (gethash (list graph-id vertex-id) *vertex-keys*)))
      most-positive-double-float))

(defun mst-previous (graph-id vertex-id)
  (first (multiple-value-list (gethash (list graph-id vertex-id) *previous*))))

;  supporto
;   mst
(defun is-mst (mst-id)
  (if (gethash mst-id *mst*) mst-id))
(defun new-mst (mst-id)
  (if (is-mst mst-id) (delete-mst mst-id))
  (new-heap mst-id (length (graph-arcs mst-id)))
  (hashtable-insert mst-id (cons 0 0) *mst*)
  mst-id)
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
  (remhash mst-id *mst*))

(defun is-visited (graph-id vertex-id)
  (second (multiple-value-list (gethash (list graph-id vertex-id) *vertex-keys*))))

(defun new-vertex-key (graph-id vertex-id weight)
  (let ((old-weight (mst-vertex-key graph-id vertex-id)))
    (cond ((> old-weight weight)
           (hashtable-insert (list graph-id vertex-id) weight *vertex-keys*))) T))

(defun new-vertex-previous (graph-id parent child)
  (hashtable-insert (list graph-id child) parent *previous*))

;   mst-prim
(defun mst-recursive (graph-id remaning-vertices fails)
  (let ((arc (second (heap-extract graph-id))))
    (if (or (null arc) (<= remaning-vertices 0)) (heap-index graph-id)
      (let ((from (third arc)) (to (fourth arc)) (weight (fifth arc)))
        (cond
         ((and (is-visited graph-id from) (is-visited graph-id to))
          (progn
            (if (> fails 15) (mst-clean-heap graph-id))
            (mst-recursive graph-id remaning-vertices (1+ fails))))
         ((is-visited graph-id from)
          (progn
            (mst-grow graph-id from to weight)
            (heap-add-arcs graph-id to)
            (mst-recursive graph-id (1- remaning-vertices) 0)))
         ((is-visited graph-id to)
          (progn
            (mst-grow graph-id to from weight)
            (heap-add-arcs graph-id from)
            (mst-recursive graph-id (1- remaning-vertices) 0))))))))

(defun mst-grow (graph-id from to weight)
  (new-vertex-key graph-id to weight)
  (new-vertex-previous graph-id from to)
  (new-vertex-key graph-id from weight))

(defun heap-add-arcs (graph-id vertex-id)
  (mapcar
   (lambda (arc)
     (if (not (is-visited graph-id (fourth arc)))
         (heap-insert graph-id (fifth arc) arc)))
   (graph-vertex-neighbors graph-id vertex-id))
  T)

;(defun mst-clean-heap (graph-id)
;  (let ((hsize (heap-index graph-id)))
;        (mst-clean-heap-recursive graph-id (heap-array graph-id) (1- hsize))
;        (format t "cleaned ~A arcs~%" (- hsize (heap-index graph-id)))
;        (buildheap graph-id)))
(defun mst-clean-heap (graph-id)
  (mst-clean-heap-recursive graph-id (heap-array graph-id) (1- (heap-index graph-id)))
  (buildheap graph-id))
(defun mst-clean-heap-recursive (heap-id array index)
  (if (>= index 0)
      (let ((arc (second (aref-strong array index)))
            (last-element-index (1- (heap-index heap-id))))
        (if (and (is-visited (second arc) (third arc)) (is-visited (second arc) (fourth arc)))
            (progn
              (swap-entries array index last-element-index)
              (array-delete-entry array last-element-index)
              (heap-decrement heap-id)))
        (mst-clean-heap-recursive heap-id array (1- index)))))

;   mst-get
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

;                            graph-id parent => arc-rep-list
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
         (get-arc-rep graph-id parent child)
         (mst-vertex-neighbors-recursive graph-id parent (rest vertices))))))

;                      arc-rep-list => arc-rep-list
(defun mst-order-arcs (arc-rep-list)
  (sort arc-rep-list #'strn< :key #'fourth)
  (stable-sort arc-rep-list #'< :key #'fifth)
  arc-rep-list)


; grafi


;  vertex-rep: ('vertex graph-id vertex-id)
;  arc-rep: ('arc graph-id vertex-id vertex-id weight)
; hashtables
;  now: graph-id -> (number-of-vertices . number-of-arcs)
(defparameter *graphs* (make-hash-table :test #'equal :size 10 :rehash-size 1))
;  next: (graph-id vertex-id) -> list of '(weight . vertex-id)
(defparameter *vertices* (make-hash-table :test #'equal :size 50000 :rehash-size 50000))

;  creazione e modifica
(defun new-graph (graph-id)
  (if (is-graph graph-id) (delete-graph graph-id))
  (hashtable-insert graph-id (cons 0 0) *graphs*)
  graph-id)
(defun delete-graph (graph-id)
  (hashtable-remove
   (lambda (key val)
     (declare (ignore val))
     (strn= (first key) graph-id))
   *vertices*)
  (remhash graph-id *graphs*)
  NIL)

(defun new-vertex (graph-id vertex-id)
  (if (is-graph graph-id)
      (if (is-vertex graph-id vertex-id)
          (make-vertex-rep graph-id vertex-id)
        (progn
          (hashtable-insert graph-id (cons (1+ (graph-vertices-n graph-id)) (graph-arcs-n graph-id)) *graphs*)
          (hashtable-insert (list graph-id vertex-id) () *vertices*)
          (make-vertex-rep graph-id vertex-id)))))

(defun new-arc (graph-id vertexS-id vertexT-id &optional (weight 1))
  (and
   (numberp weight)
   (>= weight 0)
   ;(is-vertex graph-id vertexS-id)
   ;(is-vertex graph-id vertexT-id)
   (new-vertex graph-id vertexS-id)
   (new-vertex graph-id vertexT-id)
   (if (is-arc-fluid graph-id vertexS-id vertexT-id) T
     (hashtable-insert graph-id (cons (graph-vertices-n graph-id) (1+ (graph-arcs-n graph-id))) *graphs*))
   (hashtable-insert (list graph-id vertexS-id) (cons (cons weight vertexT-id) (vertex-list graph-id vertexS-id)) *vertices*)
   (hashtable-insert (list graph-id vertexT-id) (cons (cons weight vertexS-id) (vertex-list graph-id vertexT-id)) *vertices*)
   (make-arc-rep graph-id vertexS-id vertexT-id weight)))

;  lettura
(defun is-graph (graph-id)
  (if (gethash graph-id *graphs*) graph-id))

(defun graph-vertices (graph-id)
  (mapcar
   #'make-vertex-rep-from-list
   (hashtable-get-reverse
    (lambda (key val)
      (declare (ignore val))
      (strn= (first key) graph-id))
    *vertices*)))
(defun graph-arcs (graph-id)
  (level-first-level (mapcar
   #'graph-vertex-neighbors-single-from-rep
   (graph-vertices graph-id)
   )))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (mapcar
   #'make-arc-rep-from-list
   (vertex-arcs graph-id vertex-id)))
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
;   make-rep
(defun make-vertex-rep (graph-id vertex-id)
  (list 'vertex graph-id vertex-id))
(defun make-vertex-rep-from-list (vertex-list)
  (cons 'vertex vertex-list))
(defun make-arc-rep (graph-id vertexS-id vertexT-id weight)
  (list 'arc graph-id vertexS-id vertexT-id weight))
(defun make-arc-rep-from-list (arc-list)
  (cons 'arc arc-list))

;   graph
(defun graph-vertices-n (graph-id)
  (car (gethash graph-id *graphs*)))
(defun graph-arcs-n (graph-id)
  (cdr (gethash graph-id *graphs*)))

;   vertices
;                 graph-id vertex-id => vertex, (graph-id vertex-id)
(defun is-vertex (graph-id vertex-id)
  (and
   (is-graph graph-id)
   (gethash (list graph-id vertex-id) *vertices*)
   (list graph-id vertex-id)))

;                   graph-id vertex-id => link-list, list of '(weight . vertex-id)
(defun vertex-list (graph-id vertex-id)
  (first (multiple-value-list (gethash (list graph-id vertex-id) *vertices*))))
;                          graph-id vertex-id => link-list filtered, where vertex-id < vertex-linked-id
(defun vertex-list-single (graph-id vertex-id)
  (remove-if-not (lambda (link) (strn< vertex-id (cdr link))) (vertex-list graph-id vertex-id)))

;                   graph-id vertex-id => arc-list, list of '(graph-id vertex-id vertex-id weight)
(defun vertex-arcs (graph-id vertex-id)
  (vertex-arcs-recursive graph-id vertex-id (vertex-list graph-id vertex-id)))
;                   graph-id vertex-id => arc-list filtered
(defun vertex-arcs-single (graph-id vertex-id)
  (vertex-arcs-recursive graph-id vertex-id (vertex-list-single graph-id vertex-id)))
(defun vertex-arcs-recursive (graph-id vertex-id links)
  (let ((link (first links)))
    (if (not (null link)) (cons (list graph-id vertex-id (cdr link) (car link))
                                (vertex-arcs-recursive graph-id vertex-id (rest links))))))

;                                              vertex-rep => arc-rep-list
(defun graph-vertex-neighbors-single-from-rep (vertex-rep)
  (graph-vertex-neighbors-single (second vertex-rep) (third vertex-rep)))
;                                     graph-id vertex-id => arc-rep-list
(defun graph-vertex-neighbors-single (graph-id vertex-id)
  (mapcar
   #'make-arc-rep-from-list
   (vertex-arcs-single graph-id vertex-id)))

;   arcs
;              graph-id vertexS-id vertexT-id &optional weight => weight
(defun is-arc (graph-id vertexS-id vertexT-id &optional weight)
  (if (and (is-graph graph-id) (is-vertex graph-id vertexS-id) (is-vertex graph-id vertexT-id))
   (if (null weight)
       (let ((link (first (member vertexT-id (vertex-list graph-id vertexS-id) :key #'cdr :test #'equal))))
         (if link (car link)))
     (if
         (strn= weight (car (first (member (cons weight vertexT-id) (vertex-list graph-id vertexS-id) :test #'equal))))
         weight))))
;              graph-id vertexS-id vertexT-id &optional weight => weight, check arc in both directions
(defun is-arc-fluid (graph-id vertexS-id vertexT-id &optional weight)
  (and
   (is-arc graph-id vertexS-id vertexT-id weight)
   (is-arc graph-id vertexT-id vertexS-id weight)))
;              graph-id vertexS-id vertexT-id &optional weight => weight, arcs are filtered (like in link-list-single)
(defun is-arc-single (graph-id vertexS-id vertexT-id &optional weight)
  (if (strn< vertexS-id vertexT-id) (is-arc graph-id vertexS-id vertexT-id weight)))

(defun get-arc-rep (graph-id vertexS-id vertexT-id &optional weight)
  (let ((real-weight (is-arc graph-id vertexS-id vertexT-id weight)))
    (if  real-weight (make-arc-rep graph-id vertexS-id vertexT-id real-weight))))

;                        arcs graph-id vertex-ignore => vertex-rep-list, of vertices of arcs including vertex-ignore
(defun arcs-to-vertices (arcs graph-id vertex-ignore)
  (if (not (null arcs))
      (let ((arc (first arcs)))
        (cond
         ((and (strn= (second arc) graph-id) (strn= (third arc) vertex-ignore))
          (cons (make-vertex-rep graph-id (fourth arc)) (arcs-to-vertices (rest arcs) graph-id vertex-ignore)))
         ((and (strn= (second arc) graph-id) (strn= (fourth arc) vertex-ignore))
          (cons (make-vertex-rep graph-id (third arc)) (arcs-to-vertices (rest arcs) graph-id vertex-ignore)))
         (T (arcs-to-vertices (rest arcs) graph-id vertex-ignore))))))

;   graph-print
(defun list-vertices (graph-id)
  (mapcar
   (lambda (vertex-rep) (format t "~A~%" vertex-rep))
   (graph-vertices graph-id)))
(defun list-arcs (graph-id)
  (mapcar
   (lambda (arc-rep) (format t "~A~%" arc-rep))
   (graph-arcs graph-id)))


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
          (array-delete-entry array (1- size))
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
;   heap
(defun is-heap (heap-id)
  (gethash heap-id *heaps*))
(defun heap-array (heap-id)
  (fourth (gethash heap-id *heaps*)))
(defun heap-index (heap-id)
  (let ((size (third (gethash heap-id *heaps*))))
    (if (numberp size) size 0)))

;                        heap-rep => bool, heap rep must contain same array!
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
;   heapify
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

(defun buildheap (heap-id)
  (buildheap-recursive (heap-array heap-id) (parent-idx (heap-index heap-id)) (1- (heap-index heap-id))))
(defun buildheap-recursive (array index max)
  (if (>= index 0)
      (progn
        (heapify array index max)
        (buildheap-recursive array (1- index) max))))


; supporto generico


;  alphabetically compare strings and numbers
(defun strn= (val1 val2)
  (string= (write-to-string val1) (write-to-string val2)))
(defun strn< (val1 val2)
  (if (string< (write-to-string val1) (write-to-string val2)) T))

;  hashtable shortcuts
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

;  aref that does not throw errors
(defun aref-strong (array? index)
  (if (and (arrayp array?)
           (< index (length array?))
           (>= index 0))
      (let ((value (aref array? index)))
        (if (not (strn= value 0)) value))))

;  swap used by heapify
(defun swap-entries (array i1 i2)
  (let ((e1 (aref-strong array i1))
        (e2 (aref-strong array i2)))
    (if (< (max i1 i2) (length array))
        (progn
          (if (/= i1 i2) (progn
                           (setf (aref array i1) e2)
                           (setf (aref array i2) e1)))
          array))))

(defun array-delete-entry (array index)
  (setf (aref array index) NIL))

;  make a list of lists a unique lists, just for the first level of lists inside lists
(defun level-first-level (list)
  (if list (append (first list) (level-first-level (rest list)))))
