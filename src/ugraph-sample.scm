;; graph-sample.scm

(add-to-load-path (getcwd))
(use-modules (adjlist))

(set! *random-state* (random-state-from-platform))

(define (gen-ugraph n)
  (define (ecode u v) (+ (* n u) v))
  (define selected (make-bitvector (* n n) #f))
  (let loop ((u 0))
    (when (< u n)
      (bitvector-set-bit! selected (ecode u u))
      (loop (1+ u))))
  (define adj (make-vector n '()))
  (define nedges 0)
  (define ntrials (* 2 n))
  (let loop ((k 0) (u (random n)) (v (random n)))
    (when (< k ntrials)
      (when (bitvector-bit-clear? selected (ecode u v))
        (add-undirected-edge adj u v #f)
        (bitvector-set-bit! selected (ecode u v))
        (bitvector-set-bit! selected (ecode v u))
        (set! nedges (1+ nedges)))
      (loop (1+ k) (random n) (random n))))
  (make-graph n nedges 'undirected 0 adj))
