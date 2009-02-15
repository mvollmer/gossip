;; gossip-sim - synchronous data flow simulations
;;  
;; Copyright (C) 2000, 2001, 2002  Marius Vollmer
;;  
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;  
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define-module (gossip sim-schedule)
  :use-module (gossip sim-net)
  :use-module (gossip sim-util)
  :use-module (gossip sim-options)
  :use-module (ice-9 common-list)
  :use-module (ice-9 format)
  :use-module (oop goops))

;;; Static scheduling of nets.

;; The scheduler works with BOXes and BUFs.  A BOX corresponds roughly
;; to a component and a BUF to a signal.  BUFs know how large they
;; need to be for a certain schedule and BOXes know how often they
;; should be run to fulfill the schedule.  A BOX contains either a
;; PRIMITIVE-COMPONENT or a sequence of nested BOXes.  The whole
;; schedule is represented by a top-level BOX.  The nesting of BOXes
;; does not follow the hierarchical structure of the net (although the
;; scheduler might take hints from that, but it doesn't currently).

(define-struct box () ticks out in
  (comp-id #f) (id #f) (maxticks #f) (seqno 0))

(define-struct primitive-box (box) comp)
(define-struct sequence-box (box) sequence name)

(define-method (box-name (b primitive-box))
  (comp-name (primitive-box-comp b)))

(define-method (box-name (b sequence-box))
  (sequence-box-name b))

(define (spaces n) (make-string n #\space))

(define-method (print (b box) indent)
  (let ((spc (spaces indent)))
    (format #t "~a- ~a (~a)~%" spc (box-name b) (box-ticks b))
    (format #t "~a  <" spc)
    (do-list in (box-in b)
      (format #t " ~a[~a,~a]" (box-name (buf-src in))
	      (buf-src-size in) (buf-dst-size in)))
    (format #t "~%~a  >" spc)
    (do-list out (box-out b)
      (format #t " ~a[~a,~a]" (box-name (buf-dst out))
	      (buf-src-size out) (buf-dst-size out)))
    (format #t "~%")))

(define-method (print-short (b box) indent)
  (let ((spc (spaces indent)))
    (format #t "~a~a (~a)~%" spc (box-name b) (box-ticks b))))

(define-method (print (b sequence-box) indent)
  (next-method)
  (do-list sb (sequence-box-sequence b)
    (print sb (+ indent 2))))

(define-method (print-short (b sequence-box) indent)
  (next-method)
  (do-list sb (sequence-box-sequence b)
    (print-short sb (+ indent 2))))

;; A BUF points to the underlying signal.  SRC and DST are the BOXES
;; that the BUF connects.  More precisely, SRC and DST are the
;; top-most boxes that have the BUF as an input or a output.  SRC-SIZE
;; and DST-SIZE is the chunk size of the source and destination port,
;; respectively.  The BROKEN flag denotes whether this buffer has been
;; broken to get rid of a cycle.  Only buffers belonging to delayed
;; signals will be broken.

(define-struct buf () signal src dst src-size dst-size size
                      delay (broken #f))

(define (buf-src-name b)
  (comp-name (assoc-comp (signal-src (buf-signal b)))))

(define (buf-dst-names b)
  (map (lambda (d) (comp-name (assoc-comp d)))
       (map signal-head-dst (signal-heads (buf-signal b)))))

(define (buf-src-type-size b)
  (assoc-src-type-size (signal-src (buf-signal b))))

;;; Building a list of primitive-boxes.

(define (net->boxes net)
  (let ((boxes (map (lambda (c)
		      (let ((b (make primitive-box
				 :ticks 1 :comp c :out '() :in '())))
			(set! (comp-box c) b)
			b))
		    (net-comps net))))
    (define (find-box c)  ; XXX - better algorithm
      (find-if (lambda (b) (eq? (primitive-box-comp b) c)) boxes))
    (define (signal-src-box s)
      (find-box (assoc-comp (signal-src s))))
    (define (compute-delay sh)
      (+ (* (signal-head-delay sh)
	    (assoc-dst-type-size (signal-head-dst sh)))
	 (* (signal-head-chunk-delay sh)
	    (assoc-dst-size (signal-head-dst sh)))))
    (do-list c (net-comps net)
      (let ((dst-box (find-box c)))
	(do-list a (comp-assocs c)
	  (cond
	   ((and (port? (assoc-formal a))
		 (eq? (port-dir (assoc-formal a)) 'in))
	    (do-list sh (assoc-value a)
	      (if sh
		  (let* ((s (signal-head-sig sh))
			 (src-box (signal-src-box s))
			 (b (make buf
			      :signal s
			      :src src-box :src-size (assoc-src-size 
						      (signal-src s))
			      :dst dst-box :dst-size (assoc-dst-size a)
			      :delay (compute-delay sh))))
		    (set! (signal-head-buf sh) b)
		    ;; Handle primitive cycles right here because we would
		    ;; miss them later on.
		    (cond
		     ((eq? src-box dst-box)
		      (connect-broken-buffer b))
		     (else
		      (set! (box-in dst-box)
			    (cons b (box-in dst-box)))
		      (set! (box-out src-box)
			    (cons b (box-out src-box)))))))))))))
    boxes))

;;; Box combining

(define-struct params () ticks1 ticks2 midbufs inbufs outbufs)

(define (compute-combine-params box1 box2)
  (let ((mid-bufs '())
	(in-bufs (box-in box1))
	(out-bufs (box-out box2)))
    (do-list out (box-out box1)
      (if (eq? (buf-dst out) box2)
	  (set! mid-bufs (cons out mid-bufs))
	  (set! out-bufs (cons out out-bufs))))
    (do-list in (box-in box2)
      (if (not (eq? (buf-src in) box1))
	  (set! in-bufs (cons in in-bufs))))

    (let ((p (make params
	       :midbufs mid-bufs :inbufs in-bufs :outbufs out-bufs)))
      (cond
       ((not (null? mid-bufs))
	;; Determine ticks from first mid-buf
	;;
	(let ((size (lcm (buf-src-size (car mid-bufs))
			 (buf-dst-size (car mid-bufs)))))
	  (set! (params-ticks1 p) (/ size (buf-src-size (car mid-bufs))))
	  (set! (params-ticks2 p) (/ size (buf-dst-size (car mid-bufs))))))
       (else
	(set! (params-ticks1 p) #f)
	(set! (params-ticks2 p) #f)))
      p)))

(define (combine-boxes box1 box2)

  ;; When there is a delay on a feedforward buf, the buf-size is
  ;; multiplied until the delay fits in it as well.
  ;;
  (define (round-size b s)
    (let ((d (buf-delay b)))
      (* s (1+ (quotient (+ d s -1) s)))))

  ;; Bufs are only retained as ins/outs of the seqbox when they
  ;; do not form a cycle over box1 and box2.
  ;;
  (define (retain-in-buf? buf)
    (not (eq? (buf-src buf) box2)))
  (define (retain-out-buf? buf)
    (not (eq? (buf-dst buf) box1)))

  (let ((p (compute-combine-params box1 box2)))
    (cond
     ((null? (params-midbufs p))
      #f)
     (else
      
      ;(pk 'combining (box-name box1) (box-name box2))

      ;; Check mid-bufs for rate compatibility and set their
      ;; sizes.
      ;;
      (set! (box-ticks box1) (params-ticks1 p))
      (set! (box-ticks box2) (params-ticks2 p))
      (do-list b (params-midbufs p)
	(if (not (= (* (box-ticks box1) (buf-src-size b))
		    (* (box-ticks box2) (buf-dst-size b))))
	    (error "incompatible rates")
	    (set! (buf-size b) 
		  (round-size b (* (box-ticks box1) (buf-src-size b))))))
      
      ;; Adapt chunk sizes and src/dst boxes of in/out bufs.
      ;;
      (do-list in (params-inbufs p)
	(set! (buf-dst-size in)
	      (* (buf-dst-size in) (box-ticks (buf-dst in)))))
      (do-list out (params-outbufs p)
	(set! (buf-src-size out)
	      (* (buf-src-size out) (box-ticks (buf-src out)))))

      ;; Connect broken buffers that form a loop over box1 and box2
      ;;
      (do-list b (params-inbufs p)
	(if (eq? (buf-src b) box2)
	    (connect-broken-buffer b)))
      
      ;; Finally construct sequential-box
      ;;
      (let* ((in-bufs (delete-if-not! retain-in-buf? (params-inbufs p)))
	     (out-bufs (delete-if-not! retain-out-buf? (params-outbufs p)))
	     (sbox (make sequence-box
		     :ticks 1
		     :in in-bufs
		     :out out-bufs
		     :sequence (list box1 box2)
		     :seqno (min (box-seqno box1) (box-seqno box2))
		     :comp-id (box-comp-id box1)
		     :name (format #f "(~a,~a)"
				   (box-name box1) (box-name box2)))))
	(do-list in in-bufs
	  (set! (buf-dst in) sbox))
	(do-list out out-bufs
	  (set! (buf-src out) sbox))
	sbox)))))

;; Combine two already scheduled boxes, reducing their ticks by the
;; factor FACTOR.

(define (combine-scheduled-boxes box1 box2 factor)

  ;; When there is a delay on a feedforward buf, the buf-size is
  ;; multiplied until the delay fits in it as well.
  ;;
  (define (round-size b s)
    (let ((d (buf-delay b)))
      (* s (1+ (quotient (+ d s -1) s)))))

  ;; Bufs are only retained as ins/outs of the seqbox when they
  ;; do not form a cycle over box1 and box2.
  ;;
  (define (retain-in-buf? buf)
    (not (eq? (buf-src buf) box2)))
  (define (retain-out-buf? buf)
    (not (eq? (buf-dst buf) box1)))

  (let ((p (compute-combine-params box1 box2)))
    (set! (box-ticks box1) (/ (box-ticks box1) factor))
    (set! (box-ticks box2) (/ (box-ticks box2) factor))
    (do-list b (params-midbufs p)
      (set! (buf-size b)
	    (round-size b (* (box-ticks box1) (buf-src-size b)))))
      
    ;; Construct sequential-box
    ;;
    (let* ((in-bufs (delete-if-not! retain-in-buf? (params-inbufs p)))
	   (out-bufs (delete-if-not! retain-out-buf? (params-outbufs p)))
	   (sbox (make sequence-box
		   :ticks factor
		   :in in-bufs
		   :out out-bufs
		   :sequence (list box1 box2)
		   :seqno (min (box-seqno box1) (box-seqno box2))
		   :comp-id (box-comp-id box1)
		   :name (format #f "(~a,~a)"
				 (box-name box1) (box-name box2)))))
      (do-list in in-bufs
	(cond
	 ((eq? (buf-dst in) box1)
	  (set! (buf-dst-size in)
		(* (buf-dst-size in) (box-ticks box1)))
	  (set! (buf-dst in) sbox))))
      (do-list out out-bufs
	(cond
	 ((eq? (buf-src out) box2)
	  (set! (buf-src-size out)
		(* (buf-src-size out) (box-ticks box2)))
	  (set! (buf-src out) sbox))))
      sbox)))

;; Retrieve the contents of a sequence box, which must have a tick
;; count of one, and destroy the sequence box in the process.  That
;; is, the returned boxes are no longer considered to be inside the
;; sequence box but inside the box that the sequence box was in, if
;; any.

(define (unbox-sequence-box sb)
  (let ((seq (sequence-box-sequence sb)))
    (define (find-connected-box buf ports)
      (or (find-if (lambda (box) (memq buf (ports box))) seq)
	  (error "no internal connected box found?!?")))
    (do-list in (box-in sb)
      (if (eq? (buf-dst in) sb)
	  (let ((dst (find-connected-box in box-in)))
	    (set! (buf-dst in) dst)
	    (set! (buf-dst-size in) (/ (buf-dst-size in) (box-ticks dst))))))
    (do-list out (box-out sb)
      (if (eq? (buf-src out) sb)
	  (let ((src (find-connected-box out box-out)))
	    (set! (buf-src out) src)
	    (set! (buf-src-size out) (/ (buf-src-size out) (box-ticks src))))))
    (set! (sequence-box-sequence sb) #f)
    ;(for-each (lambda (b) (print b 0)) seq)
    seq))

(define (list*-non-null . args)
  (cond
   ((null? args)
    '())
   ((null? (cdr args))
    (car args))
   ((null? (car args))
    (apply list*-non-null (cdr args)))
   (else
    (cons (car args) (apply list*-non-null (cdr args))))))

;;; Connecting broken signals.

(define (connect-broken-buffer buf)

  (define (round-size b s)
    (let ((d (buf-delay b)))
      (* s (1+ (quotient (+ d s -1) s)))))

  (let ((sig (buf-signal buf)))
    ;;(format #t ";;; signal ~a is broken~%" (signal-name sig))
    ;;(format #t ";;; src-size ~a, dst-size ~a, delay ~a~%"
    ;;	    (buf-src-size buf)
    ;;	    (buf-dst-size buf)
    ;;	    (buf-delay buf))
    (if (not (= (buf-src-size buf) (buf-dst-size buf)))
	(error (format #f "rate mismatch on signal ~a"
		       (signal-name sig))))
    (if (< (buf-delay buf) (buf-src-size buf))
	(error
	 (format #f "too little delay on signal ~a, need at least ~a"
		 (signal-name sig) (/ (buf-src-size buf)
				      (buf-src-type-size buf)))))
    (set! (buf-size buf) (round-size buf (buf-src-size buf)))
    (set! (buf-broken buf) #f)))
  
;;; Actual scheduler

;; The scheduler works by topologically sorting the net, identifying
;; strongly connected components, breaking selected signals to convert
;; the component into a cycle free graph, and recursing on that graph,
;; reconnecting the broken signals on the way out.

(define true? identity)

(define-macro (time tag . forms)
  `(let* ((thunk (lambda () ,@forms))
	  (start (get-internal-run-time))
	  (result (thunk))
	  (stop (get-internal-run-time)))
     (pk ,tag (/ (- stop start) internal-time-units-per-second) 'seconds)
     result))
     
(define (compute-max-ticks box)
  (let ((max-ticks (pick-mappings (lambda (b)
				    (let ((d (buf-delay b)))
				      (if (and (buf-broken b) (> d 0))
					  (/ d (buf-src-size b))
					  (let ((m (box-maxticks (buf-src b))))
					    (if (eq? m #t)
						#f
						(/ (* (buf-src-size b) m)
						   (buf-dst-size b)))))))
				  (box-in box))))
    (if (null? max-ticks)
	#t
	(apply min max-ticks))))

(define (buf-unbroken? b)
  (not (buf-broken b)))

(define (list-front! start end)
  (let loop ((l start))
    (cond
     ((eq? (cdr l) end)
      (set-cdr! l '()))
     (else
      (loop (cdr l)))))
  start)

(define (find-schedule boxes)
  (let ((comp-id 0)
	(seqno 0)
	(waiting '())
	(n-considered 0)
	(n-unsuccessful 0)
	(n-indirect-sources 0)
	(n-not-combinable 0))

    ;; Is S an indirect source for D?
    ;;
    (define (indirect-source? s d)
      (define (reachable? s path)
	(cond
	 ((eq? s d)
	  (> (length path) 1))
	 ((or (not (box-seqno s))
	      (> (box-seqno s) (box-seqno d)))
	  #f)
	 ((memq s path)
	  #f)
	 (else
	  (let ((path (cons s path)))
	    (or-map (lambda (s2) (reachable? s2 path))
		    (map buf-dst (pick buf-unbroken? (box-out s))))))))
      (reachable? s '()))

    ;; Two boxes are combinable when the resulting tick count will not
    ;; exceed the max tick count of either box.
    ;;
    (define (combinable? buf)
      (define (le x y) (or (eq? y #t) (<= x y)))
      (let ((s (lcm (buf-src-size buf) (buf-dst-size buf))))
	(le (/ s (buf-src-size buf)) (box-maxticks (buf-src buf)))
	(le (/ s (buf-dst-size buf)) (box-maxticks (buf-dst buf)))))

    ;; Consider B for combination with the other boxes.
    ;;
    (define (consider b)
      (set! waiting (cons b waiting))
      (consider1 b))

    (define (consider1 b)

      ;; A candidate for B is a box that is a direct source for B but
      ;; not a strictly indirect source, and is combinable.

      (define (find-candidate)
	(let loop ((bufs (box-in b))
		   (non-cands '()))
	  (cond 
	   ((null? bufs)
	    #f)
	   ((buf-broken (car bufs))
	    (loop (cdr bufs) non-cands))
	   (else
	    (let ((s (buf-src (car bufs))))
	      (cond
	       ((memq s non-cands)
		(loop (cdr bufs) non-cands))
	       ((not (combinable? (car bufs)))
		(set! n-not-combinable (1+ n-not-combinable))
		(loop (cdr bufs) (cons s non-cands)))
	       ((indirect-source? s b)
		(set! n-indirect-sources (1+ n-indirect-sources))
		(loop (cdr bufs) (cons s non-cands)))
	       (else
		s)))))))

      (set! n-considered (1+ n-considered))

      (cond ((not (box-seqno b))
	     (set! (box-seqno b) seqno)
	     (set! seqno (1+ seqno))))
      (if (not (eq? (box-maxticks b) #t))
	  (set! (box-maxticks b) (compute-max-ticks b)))
      ;(format #t "~a: ~a: ~a~%" (box-seqno b) (box-name b) (box-maxticks b))
      (let ((b2 (find-candidate)))
	(cond
	 (b2
	  (let ((sb (combine-boxes b2 b)))
	    (set! waiting (delq! b (delq! b2 waiting)))
	    (consider sb)))
	 (else
	  (set! n-unsuccessful (1+ n-unsuccessful))
	  #f))))

    ;; Finish off by combining all remaining boxes (which
    ;; must be unconnected) into one seqbox.
    ;;
    (define (make-top-seqbox)
      (cond
       ((null? (cdr waiting))
	(car waiting))        ; only one element, use it directly
       (else
	;; Try to connect the remaining boxes forcefully; this should
	;; either be a noop or throw an error.
	(do-list b waiting
	  (set! (box-maxticks b) #t))
	(do-list b waiting
	  (consider1 b))
	;; They are all unconnected or we wouldn't be here.
	(let ((top (make sequence-box
		     :ticks 1 :in '() :out '()
		     :sequence waiting
		     :name (format #f "comp~a" comp-id))))
	    (do-list b waiting
	      (set! (box-ticks b) 1))
	    top))))
    
    ;; Schedule the BOXES, without paying attention to connections to
    ;; other boxes in ALL-BOXES.
    ;;
    (define (consider-component boxes comp-id)
      ;; (pk 'on (map box-name boxes) comp-id)
      (let ((stack '())
	    (id 0))

	(define (visit-src buf)
	  (if (and (not (buf-broken buf))
		   (= comp-id (box-comp-id (buf-src buf))))
	      (visit (buf-src buf))
	      #t))

	(define (min-id m lst)
	  (apply min m (pick number? lst)))

	(define (visit b)
	  (or (box-id b)
	      (begin
		(set! (box-id b) id)
		(set! id (1+ id))
		(let ((old-stack stack))
		  (set! stack (cons b stack))
		  (let ((m (min-id (box-id b)
				   (map visit-src (box-in b)))))
		    (cond
		     ((= m (box-id b))
		      (let ((comp (list-front! stack old-stack)))
			(consider-strongly-connected comp)
			(set! stack old-stack)
			;; XXX - not needed because boxes now belong
			;; to another component anyway?
			(for-each (lambda (b)
				    (set! (box-id b) #t))
				  comp))))
		    m)))))

	(for-each (lambda (b)
		    (set! (box-comp-id b) comp-id)
		    (set! (box-id b) #f))
		  boxes)
	(for-each visit boxes)))
    
    ;; Consider a strongly connected component.  We do this by breaking
    ;; out all `leaders' and thereby opening all cycles that make the
    ;; component strongly connected.  A `leader' is a box in the
    ;; component that has only `breakable' intra-component input
    ;; buffers.  A buffer is breakable when it has a positive delay.
    ;;
    (define (consider-strongly-connected boxes)
      (define (breakable? buf)
	(> (buf-delay buf) 0))
      (define (inner-bufs bufs)
	(let ((b (pick (lambda (b)
			 (= (box-comp-id (buf-src b))
			    (box-comp-id (buf-dst b))))
		       bufs)))
	  ;; (pk 'inner (map (lambda (bb)
	  ;; (cons (signal-name (buf-signal bb))
	  ;;			  (buf-delay bb)))
	  ;;		  b))
	  b))
      (define (consider-for-breaking box)
	;; (pk 'considering-break (box-name box))
	(let ((bufs (inner-bufs (box-in box))))
	  (cond
	   ((every breakable? bufs)
	    ;; (pk 'breaking (map (lambda (b) (signal-name (buf-signal b)))
	    ;;		       bufs))
	    (do-list b bufs
	      (set! (buf-broken b) #t))
	    #t)
	   (else
	    #f))))
      ;; (pk 'strong (map box-name boxes))
      (set! comp-id (1+ comp-id))
      (do-list b boxes
	(set! (box-comp-id b) comp-id))
      (cond
       ((null? (cdr boxes))
	(consider (car boxes)))
       (else
	(if (some true? (map consider-for-breaking boxes))
	    (consider-component boxes comp-id)
	    (error "dead-lock among" (map box-name boxes))))))

    (for-each (lambda (b) (set! (box-seqno b) #f)) boxes)
    (pk 'scheduling (length boxes) 'boxes)
    (time 'time
     (consider-component boxes comp-id))
    ;;(pk 'considered n-considered 'unsuccessfully n-unsuccessful)
    ;;(pk 'indirect-sources n-indirect-sources 
    ;;    'not-combinable n-not-combinable)
    (make-top-seqbox)))

;;; Peephole Optimizer

;; We perform a peephole optimization on the schedule in a recursive
;; manner.  For a primitive box, do nothing; for a sequence box,
;; optimize the contained boxes, and then try to optimize the sequence
;; itself, using `elementary optimizations'.
;;
;; The elementary optimizations currently are:
;;
;; - absorbing of subordinate sequence boxes with a tick of 1 into the
;;   containing sequence.
;;
;; - for two adjacent blocks in the sequence with
;;  
;;     f = gcd(ticks1, ticks2) > 1
;;
;;   replace them with a sequence box...
;;
;; More to follow.
;;
;; The input schedule is destructively modified in place.

(define (optimize-schedule box)
  (cond
   ((sequence-box? box)
    (let ((seq (apply append!
		      (map
		       (lambda (b)
			 (optimize-schedule b)
			 (cond
			  ((and (sequence-box? b) (= (box-ticks b) 1))
			   (unbox-sequence-box b))
			  (else
			   (list b))))
		       (sequence-box-sequence box)))))
      (let loop ((s seq)
		 (r '()))
	(cond ((null? s)
	       (set! (sequence-box-sequence box) (reverse! r)))
	      ((null? (cdr s))
	       (loop (cdr s) (cons (car s) r)))
	      (else
	       (let* ((b1 (car s))
		      (b2 (cadr s))
		      (f (gcd (box-ticks b1) (box-ticks b2))))
		 (cond
		  ((> f 1)
		   (let ((sb (combine-scheduled-boxes b1 b2 f)))
		     (optimize-schedule sb)
		     (loop (cons sb (cddr s)) r)))
		  (else
		   (loop (cdr s) (cons (car s) r))))))))))))

;;; Main entry point

(define-method (print (b buf) indent)
  (format #t "~a~a -> ~a: ~a~%" 
	  (spaces indent) (buf-src-name b) 
	  (map string->symbol (buf-dst-names b)) (buf-size b)))

(define (buffer-space net)
  (let ((total 0))
    (do-list sig (net-sigs net)
      (do-list sighead (signal-heads sig)
	(let ((maxbuf (apply max 
			     (map buf-size 
				  (map signal-head-buf 
				       (signal-heads sig))))))
	  (set! total (+ total maxbuf)))))
    total))
  
(define (schedule net)
  (let ((top-box (find-schedule (net->boxes net))))
    ;(print top-box 0)
    (pk 'optimizing (buffer-space net) 'bytes)
    (time 'time
      (optimize-schedule top-box))
    (pk 'buffers (buffer-space net) 'bytes)
    (cond ((verbose?)
	   (format #t "~%Schedule:~%")
	   (print-short top-box 1)
	   (let ((total 0))
	     (format #t "~%Buffers:~%")
	     (do-list sig (reverse (net-sigs net))  ;; XXX - take maximum
	       (do-list sighead (signal-heads sig)
		 (format #t " ~a -> ~a: ~a~%" 
			 (comp-name (assoc-comp (signal-src sig)))
			 (comp-name (assoc-comp (signal-head-dst sighead)))
			 (buf-size (signal-head-buf sighead))))
	       (let ((maxbuf (apply max 
				    (map buf-size 
					 (map signal-head-buf 
					      (signal-heads sig))))))
		 (if (> (length (signal-heads sig)) 1)
		     (format #t "  max: ~a~%" maxbuf))
		 (set! total (+ total maxbuf))))
	     (format #t " total: ~a bytes~%~%" total))))
    top-box))

(export schedule)
