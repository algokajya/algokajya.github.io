
(define-module (adjlist)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (
            make-graph
            get-num-nodes
            set-num-nodes!
            get-num-edges
            set-num-edges!
            get-root
            set-root!
            get-adj-vector
            set-adj-vector!
            add-directed-edge
            add-undirected-edge
            for-each-node-of
            for-each-neighbor-of
            show-neighbors
            show-graph
            make-graphviz
            add-edge-proc
            ))

;;
;;  A vector of adjecency lists here looks like this: 
;; 
;;      adj  
;;     |   |
;;      ---
;;  id | +-|-->  ( (id_1 . w_1) (id_2 . w_2) ... (id_n . w_n) )
;;      --- 
;;     |   |
;;
;;  where id and id_i are node names which are also indices of adj, 
;;    and w_i is a weight of edge connecting id to id_i.   

(define-record-type <graph>
  (make-graph number-of-nodes 
              number-of-edges 
              type            ;; 'undirected or 'directed
              root            ;; root node 
              adj)            ;; a vector of adjecency lists
  graph?
  (number-of-nodes get-num-nodes  set-num-nodes!)
  (number-of-edges get-num-edges  set-num-edges!)
  (type            get-type       set-type!)
  (root            get-root       set-root!)
  (adj             get-adj-vector set-adj-vector!))

(define-syntax-rule (undirected? G) 
  (eq? (get-type G) 'undirected))

(define-syntax-rule (directed? G) 
  (eq? (get-type G) 'directed))

(define-syntax-rule (add-directed-edge adj u v weight)
  (vector-set! adj u (cons (cons v weight) (vector-ref adj u))))

(define-syntax-rule (add-undirected-edge adj u v weight)
  (begin 
    (vector-set! adj u (cons (cons v weight) (vector-ref adj u)))
    (vector-set! adj v (cons (cons u weight) (vector-ref adj v)))))

(define-syntax-rule (for-each-node-of G proc)
  (vector-for-each proc (get-adj-vector G)))

(define-syntax-rule (for-each-neighbor-of neighbors proc)
  (for-each proc neighbors))

(define (show-neighbors G)
  (define fmt 
    (string-append "adj[~A] " (if (undirected? G) "--" "->") " ~A\n"))
  (lambda (u neighbors) (format #t fmt u neighbors)))

(define* (show-graph G #:optional (print-neighbors #t))
  (format #t "The # of nodes:~A\n" (get-num-nodes G))
  (format #t "The # of edges:~A\n" (get-num-edges G))
  (format #t "graph type:~A\n" (get-type G))
  (format #t "The root:~A\n" (get-root G))
  (when print-neighbors (for-each-node-of G (show-neighbors G))))

;;
;; Using Graphviz, this produces a png file of graph G.  
;;
(define (make-graphviz G fname)
  (define gv-file (string-append fname ".gv"))
  (define png-file (string-append fname ".png"))
  (call-with-output-file gv-file
    (lambda (port)
      (format port "~A {\n" (if (undirected? G) "graph" "digraph"))
      (for-each-node-of 
       G
       (lambda (u neighbors)
         (for-each-neighbor-of 
          neighbors 
          (lambda (neighbor) 
            (let ((id (car neighbor)) (wght (cdr neighbor)))
              (if (undirected? G) 
                  (when (< u (car neighbor))
                    (format port "  ~A--~A[label=\"~A\"];\n" u id 
                            (if wght (number->string wght) "")))
                  (format port "  ~A->~A[label=\"~A\"];\n" u id
                          (if wght (number->string wght) ""))
                  ))))))
      (format port "}\n")
      ))
  (system (string-append "dot -Tpng " gv-file " -o " png-file)))


(define-syntax-rule (get-random-weight lb ub)
  (+ lb (random (- ub lb))))

;;
;; Produce a procedure for adding an edge to adjecency vector.
;;   directed 
;;     = #t ==> add directed edges 
;;     = #f ==> add undirected edges
;;   reverse
;;     = #t ==> add directed edges with direction toward the root
;;     = #f ==> add directed edges with direction away from the root.  
;;     If directed=#f, this is ignored.
;;   weight-interval 
;;     = #f ==> every edge has no weight. 
;;     = a pair (lb . ub) of integers such that lb < ub
;;          ==> every edge has an integer weight which is chosen randomly 
;;              between lb(inclusive) and ub(exclusive).
;;

(define (add-edge-proc directed reverse weight-interval)
  (if directed 
      (directed-edge-proc weight-interval reverse)
      (undirected-edge-proc weight-interval)))

(define (directed-edge-proc weight-interval reverse)
  (if weight-interval
      (let ((lb (car weight-interval)) 
            (ub (cdr weight-interval)))
        (if reverse 
            (lambda (adj u v) 
              (add-directed-edge adj v u (get-random-weight lb ub)))
            (lambda (adj u v) 
              (add-directed-edge adj u v (get-random-weight lb ub)))))
      (if reverse 
          (lambda (adj u v) (add-directed-edge adj v u #f))
          (lambda (adj u v) (add-directed-edge adj u v #f)))))

(define (undirected-edge-proc weight-interval)
  (if weight-interval
      (let ((lb (car weight-interval)) 
            (ub (cdr weight-interval)))
        (lambda (adj u v) 
          (let ((w (get-random-weight lb ub)))
            (add-undirected-edge adj u v w))))
      (lambda (adj u v) (add-undirected-edge adj u v #f))))

