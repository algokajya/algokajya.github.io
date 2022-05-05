
;; time-utils.scm

(define-module (time-utils)
  #:export (
            internal-time-unit->second
            internal-time-unit->milisecond
            measure-real-time
            measure-real-time-second
            measure-real-time-milisecond
            measure-run-time
            measure-run-time-second
            measure-run-time-milisecond
            measure-time
            measure-time-second
            measure-time-milisecond
            )
)

;; 内部時間単位を秒単位に変換する．
(define (internal-time-unit->second t)
  (exact->inexact (/ t internal-time-units-per-second)))

;; 内部時間単位をミリ秒単位に変換する．
(define (internal-time-unit->milisecond t)
  (exact->inexact (* (/ t internal-time-units-per-second) 1000)))

;; サンク thunk の実時間を返す．
(define (measure-real-time thunk)
  (define start-time #f) 
  (define end-time #f)
  (set! start-time (get-internal-real-time))
  (thunk)
  (set! end-time (get-internal-real-time))
  (- end-time start-time))

(define (measure-real-time-second thunk)
  (internal-time-unit->second (measure-real-time thunk)))

(define (measure-real-time-milisecond thunk)
  (internal-time-unit->milisecond (measure-real-time thunk)))

;; サンク thunk のCPU時間を返す．
(define (measure-run-time thunk)
  (define start-time #f) 
  (define end-time #f)
  (set! start-time (get-internal-run-time))
  (thunk)
  (set! end-time (get-internal-run-time))
  (- end-time start-time))

(define (measure-run-time-second thunk)
  (internal-time-unit->second (measure-run-time thunk)))

(define (measure-run-time-milisecond thunk)
  (internal-time-unit->milisecond (measure-run-time thunk)))

;; サンク thunk の実時間とCPU時間を返す．
(define (measure-time thunk)
  (define start-real-time #f) 
  (define end-real-time #f)
  (define start-run-time #f) 
  (define end-run-time #f)
  (set! start-real-time (get-internal-real-time))
  (set! start-run-time (get-internal-run-time))
  (thunk)
  (set! end-run-time (get-internal-run-time))
  (set! end-real-time (get-internal-real-time))
  (vector 
   (- end-real-time start-real-time)
   (- end-run-time start-run-time)))

(define (measure-time-second thunk)
  (let ((tm (measure-time thunk)))
    (vector 
     (internal-time-unit->second (vector-ref tm 0))
     (internal-time-unit->second (vector-ref tm 1)))))

(define (measure-time-milisecond thunk)
  (let ((tm (measure-time thunk)))
    (vector 
     (internal-time-unit->milisecond (vector-ref tm 0))
     (internal-time-unit->milisecond (vector-ref tm 1)))))
