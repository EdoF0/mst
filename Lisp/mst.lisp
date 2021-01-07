;;creazione hash-tables
(defparameter *vertices* (make-hash-table :test #' equal))
(defparameter *arcs* (make-hash-table :test #' equal))
(defparameter *graphs* (make-hash-table :test #' equal))
(defparameter *visited* (make-hash-table :test #' equal))
(defparameter *vertex-keys* (make-hash-table :test #' equal))
(defparameter *previous* (make-hash-table :test #' equal))
(defparameter *heaps* (make-hash-table :test #' equal))


;GRAFI
;;  Creazione e modifica

(defun new-graph (graph-id)
  (delete-graph graph-id)
  (setf (gethash graph-id *graphs*) graph-id))

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
    (setf (gethash (list 'vertex graph-id vertex-id)
                 *vertices*)
        (list 'vertex graph-id vertex-id))))

(defun new-arc (graph-id vertexS-id vertexT-id &optional (weight 1))
    (and 
      (vertex-in-graph graph-id vertexS-id)
      (vertex-in-graph graph-id vertexT-id)
      (setf (gethash (list 'arc graph-id vertexS-id vertexT-id) *arcs*)
            (list 'arc graph-id vertexS-id vertexT-id weight))))

;;  Lettura

(defun graph-vertices (graph-id)
  (let ((vertices  ()))  
    (maphash (lambda (key val)
               (if (strn= (second key) graph-id)
                   (push val vertices)
                 ))
             *vertices*)
    vertices))

(defun graph-arcs (graph-id)
  (let ((arcs ()))  
    (maphash (lambda (key val)
               (if (strn= (second key) graph-id)
                   (push val arcs)
                 ))
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
                   (push val arcs)
                 ))
             *arcs*)
    arcs))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (let ((arcs ()) (vertices ())) 
    (maphash (lambda (key val)
               (if (and (strn= (second key) graph-id) 
                        (or 
                         (strn= (third key) vertex-id) 
                         (strn= (fourth key) vertex-id)))
                   (push val arcs)
                 ))
             *arcs*)
    (mapcar (lambda (arc)
              (if (equal (third arc) vertex-id)
                  (push (list 'vertex graph-id (fourth arc)) vertices)
                (push (list 'vertex graph-id (third arc)) vertices)))
            arcs)
    vertices))


;; Stampa

(defun graph-print (graph-id)
  (format t "VERTICI:~%") 
  (list-vertices graph-id)
  (format t "~%ARCHI:~%")
  (list-arcs graph-id))

;;  Supporto

(defun beautify-arc (arc parent)
  (cond ((strn= (third arc) parent) arc) 
        (T (list 'arc (second arc) parent (third arc) (fifth arc)))))


(defun list-vertices (graph-id)
  (maphash (lambda (key val)
             (if (strn= (second key) graph-id)
                 (format t "~A~%" val)
               ))
           *vertices*))

(defun list-arcs (graph-id)
  (maphash (lambda (key val)
             (if (strn= (second key) graph-id)
                 (format t "~A~%" val)
               ))
           *arcs*))

(defun vertex-in-graph (graph-id vertex-id) 
  (and 
  (is-graph graph-id)
  (gethash (list 'vertex graph-id vertex-id) *vertices*)))

;MinHeap
;;  Creazione e mofica

(defun new-heap (heap-id &optional (capacity 42))
      (setf (gethash heap-id *heaps*)
            (list 'heap heap-id 0 (make-array capacity)))

(defun heap-delete (heap-id) 
  (remhash heap-id *heaps*)
  T)

(defun heap-insert (heap-id key val)
  (let ((loheap (gethash heap-id *heaps*)))
  (if (and (not (null loheap)) (< (heap-size heap-id) (heap-maxsize heap-id)))
      (progn 
        (setf 
         (aref (fourth loheap) 
                (heap-size heap-id)) 
         (list key val)) 
        (let ((current (heap-size heap-id)))
        (recursive-insert heap-id current)
        (setf (third loheap) (+ (heap-size heap-id) 1))
        T)) 
    nil)))

(defun heap-extract (heap-id) 
  (if (<= (heap-size heap-id) 0)
      ()
    (let ((loheap (heap-actual-heap heap-id)))
      (if (not (null loheap))
        (let ((head (aref loheap 0)))
          (setf (aref loheap 0) (aref loheap (- (heap-size heap-id) 1)))
          (setf (aref loheap (- (heap-size heap-id) 1)) nil)
          (setf (third (gethash heap-id *heaps*)) (- (heap-size heap-id) 1))
          (minheapify heap-id 0)
          head)
        ()))))


;;  Lettura

(defun heap-empty (heap-id)
  (let ((size (heap-size heap-id)))
    (if size (zerop size))))

(defun heap-not-empty (heap-id)
  (let ((size (heap-size heap-id)))
    (if size (> size 0))))

(defun heap-head (heap-id)
  (and 
  (gethash heap-id *heaps*)
  (aref (heap-actual-heap heap-id) 0)))

;;  Stampa

(defun heap-print (heap-id)
  (if (null (heap-size heap-id))
      nil
    (progn
     (format t "Lo heap contine ~D elementi: ~A"
         (heap-size heap-id) (heap-actual-heap heap-id)) 
     T)))

;;  Supporto


(defun heap-size (heap-id)
  (and (gethash heap-id *heaps*)
  (third (gethash heap-id *heaps*))))


(defun heap-actual-heap (heap-id) 
  (and (gethash heap-id *heaps*)
       (fourth (gethash heap-id *heaps*))))

(defun parent (index)
  (first (multiple-value-list (floor (/ index 2))))) 

(defun leftchild (index)
  (* index 2))

(defun rightchild (index)
  (+ (* index 2) 1))

(defun isleaf (heap-id index)
 (and (>= index (first 
                 (multiple-value-list 
                  (floor (/ (heap-size heap-id) 2)))))
      (<= index (heap-size heap-id))))
      

(defun swap (index1 index2 heap-id)
  (let ((loheap (heap-actual-heap heap-id))) 
      (let ((temp (aref loheap index1)))  
  (setf (aref loheap index1) 
        (aref loheap index2))
  (setf (aref loheap index2)
        temp))))

(defun minheapify (heap-id index)
  (let ((loheap (heap-actual-heap heap-id))) 
  (if (null (isleaf heap-id index))
      (let ((indexel (first (aref loheap index))) 
            (leftchildel (first (aref loheap (leftchild index))))
            (rightchildel (first (aref loheap (rightchild index)))))
      (if (or (> indexel leftchildel)
              (> indexel rightchildel))
          (if (< leftchildel rightchildel) 
              (progn 
                (swap index (leftchild index) heap-id) 
                (minheapify heap-id (leftchild index)))
            (progn 
              (swap index (rightchild index) heap-id) 
              (minheapify heap-id (rightchild index)))))) T)))

(defun heap-maxsize (heap-id) 
  (length (heap-actual-heap heap-id)))
(defun recursive-insert (heap-id current)
  (let ((loheap (heap-actual-heap heap-id)))
    (if 
      (< (first (aref loheap
            current)) 
         (first (aref loheap
            (parent current)))) 
      (progn 
        (swap current (parent current) heap-id)
        (recursive-insert 
         heap-id (parent current))))))




;MST

;;  Esecuzione
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

;;  Dati

(defun mst-vertex-key (graph-id vertex-id) 
  (or (gethash (list graph-id vertex-id) *vertex-keys*) most-positive-double-float))

(defun mst-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

;; Supporto


(defun delete-mst (graph-id)
  (maphash (lambda (key val) (if (strn= (first key) graph-id) (remhash key *vertex-keys*))) *vertex-keys*)
  (maphash (lambda (key val) (if (strn= (first key) graph-id) (remhash key *previous*))) *previous*)
  (maphash (lambda (key val) (if (strn= (first key) graph-id) (remhash key *visited*))) *visited*))




(defun mst-recursive (graph-id)
  (let ((head (heap-extract graph-id)))
    (if (null head)
        t
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
  (if (null ordered-arcs)
      nil
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
               (if (strn= val parent)
                   (push (second key) children)))
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
           (setf (gethash (list graph-id vertex-id) *vertex-keys*) weight))) T))

(defun new-vertex-visited (graph-id vertex-id)
  (setf (gethash (list graph-id vertex-id) *visited*) T))

(defun new-vertex-previous (graph-id parent child)
  (setf (gethash (list graph-id child) *previous*) parent))


(defun heap-add-arcs (graph-id vertex-id)
 (mapcar (lambda (arc) (heap-insert graph-id (fifth arc) arc)) 
         (graph-vertex-neighbors graph-id vertex-id))
 T)

; Funzioni di supporto generiche

(defun strn= (val1 val2) 
         (string= (write-to-string val1) (write-to-string val2)))

(defun strn< (val1 val2)
   (string< (write-to-string val1) (write-to-string val2)))