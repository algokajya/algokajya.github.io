
(define-module (gentree)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-43)
  #:export (
            random-walk-tree
            random-Adlous-tree
            random-Wilson-tree
            ))

(add-to-load-path (getcwd))
(use-modules (adjlist))

(set! *random-state* (random-state-from-platform))

;;;;;;;;;;;;;;;;;;;;
;;
;; Generate a labeled tree with random walk on the complete graph.
;;
;; [Note] 
;; It is known that the algorithm generates a labeled tree uniformly at random.  
;; See the following paper[Proposition 1 or the paragraph just before Algorithm 2].
;;    David Aldous: The Random Walk Construction of Uniform Spanning Trees 
;;    and Uniform Labelled Trees, SIAM J. Discrete Math. Vol.3, Iss.4 (1990), 
;;    450-465.  
;; The stochastic nature of the algorithm was also investigated in the following 
;; paper: 
;;    A. Broder. Generating random spanning trees, Proceedings of the 30th 
;;    Annual IEEE Symposium on Foundations of Computer Science, October 1989, 
;;    pp.442-447. 
;; At least on the complete graph of n nodes, it is not difficult to show 
;; that the algorithm runs in O(nlog(n)) time in average. Experiences support 
;; this fact. For a detailed analysis on the time complexity, refer to Theorem 5
;; of the following paper.
;;    Andrei Z. Broder Anna R. Karlin: Bounds On The Cover Time, 
;;    SRC Research Report 32, Octob er 15, 1988. 
;;

(define* (random-walk-tree num-of-nodes 
                           #:key 
                           (directed #f) 
                           (reverse #f)
                           (weight-interval #f))
  (define num-of-edges (- num-of-nodes 1))
  (define root (random num-of-nodes))
  (define adj (make-vector num-of-nodes '()))
  (define visited (make-bitvector num-of-nodes #f))
  (bitvector-set-bit! visited root)
  (define add-edge (add-edge-proc directed reverse weight-interval))
  (define num-of-nodes-1 (1- num-of-nodes))
  (define-syntax-rule (get-neighbor u)
    (let ((v (random num-of-nodes-1))) 
      (if (< v u) v (1+ v))))
  (let random-walk ((k 1) 
                    (u root) 
                    (v (get-neighbor root)))
    (when (< k num-of-nodes)
      (if (bitvector-bit-clear? visited v)
          (begin 
            (bitvector-set-bit! visited v)
            (add-edge adj u v)
            (random-walk (1+ k) v (get-neighbor v)))
          (random-walk k v (get-neighbor v)))))
  (make-graph num-of-nodes 
              num-of-edges 
              (if directed 'directed 'undirected)
              root 
              adj))


;;;;;;;;;;;;;;;;;;;;
;; 
;; Another algorithm to generate a random labeled tree, which is described 
;; in Adlous's paper mentioned above. This runs obviously in linear time.
;; See Algorithm 2 and Proposision 3 of the paper.
;;

(define* (random-Adlous-tree num-of-nodes 
                             #:key 
                             (directed #f) 
                             (reverse #f)
                             (weight-interval #f))
  (define num-of-edges (- num-of-nodes 1))
  (define adj (make-vector num-of-nodes '()))
  (define add-edge (add-edge-proc directed reverse weight-interval))
  (define perm 
    (let ((vec (vector-unfold (lambda (v) v) num-of-nodes)))
      (let loop ((v (- num-of-nodes 1)))
        (when (> v 0)
          (vector-swap! vec v (random (+ v 1)))
          (loop (1- v))))
      vec))
  (define-syntax relabel-node 
    (syntax-rules ()
      ((_ x) (vector-ref perm x))))
  (let loop ((v 1))
    (when (< v num-of-nodes)
      (add-edge adj
                (relabel-node (min (- v 1) (random num-of-nodes)))
                (relabel-node v)
                )
      (loop (1+ v))))
  (define root (relabel-node 0))
  (make-graph num-of-nodes 
              num-of-edges 
              (if directed 'directed 'undirected)
              root 
              adj))



;;;;;;;;;;;;;;;;;;;;
;; 
;; Another algorithm to generate a random labeled tree, which is described 
;; in the following paper. 
;;   David Bruce Wilson: Generating Random Spanning Trees More Quickly than 
;;   the Cover Time, Proceedings of the twenty-eighth annual ACM symposium 
;;   on Theory of Computing, May 1996, pp.296-303. 
;;

(define* (random-Wilson-tree num-of-nodes 
                           #:key 
                           (directed #f) 
                           (reverse #f)
                           (weight-interval #f))
  (define num-of-edges (- num-of-nodes 1))
  (define root (random num-of-nodes))
  (define adj (make-vector num-of-nodes '()))
  (define add-edge (add-edge-proc directed reverse weight-interval))
  (define num-of-nodes-1 (1- num-of-nodes))
  (define-syntax-rule (get-neighbor u)
    (let ((v (random num-of-nodes-1))) 
      (if (< v u) v (1+ v))))
  (define visited (make-bitvector num-of-nodes #f))
  (bitvector-set-bit! visited root)
  (define next (make-vector num-of-nodes #f))
  (let loop ((i 0))
    (when (< i num-of-nodes)
      (let walk-loop ((u i))
        (when (bitvector-bit-clear? visited u)
          (let ((v (get-neighbor u)))
            (vector-set! next u v)
            (walk-loop v))))
      (let edge-loop ((u i))
        (when (bitvector-bit-clear? visited u)
          (bitvector-set-bit! visited u)
          (edge-loop (vector-ref next u))))
      (loop (1+ i))))
  (let loop ((u 0))
    (when (< u num-of-nodes)
      (and=> (vector-ref next u)
             (lambda (v) (add-edge adj v u)))
      (loop (1+ u))))
  (make-graph num-of-nodes 
              num-of-edges 
              (if directed 'directed 'undirected)
              root 
              adj))

