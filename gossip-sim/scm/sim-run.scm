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

(define-module (gossip sim-run)
  :use-module (gossip sim-net)
  :use-module (gossip sim-schedule)
  :use-module (gossip sim-util)
  :use-module (gossip sim-builtin)
  :use-module (gossip sim-options)
  :use-module (ice-9 format)
  :no-backtrace)

(export run-simulation)

;;; Converting the net to the C side representation

;; c-structure: (comp...) [not sorted in any way]
;; comp: (dyncomp ins outs)
;; ins outs: (sig...) [sorted according to interface]
;; sig: (dyncomp index read-speed write-speed)

;;; Multi-Ports

;; The C side does not support multi-ports in their full generality
;; right now.  Only the last input/output port of a block can be
;; flagged as being `multi'.  All ports are identified by a index
;; number, and the last port will simply gather all connections that
;; specify a port index greater than the declared number of ports.

; (define (net->cstruct net)
;  
;   (define (signal-id s peer)
;     ;; Handle multi-ports in the way that the C side expects them.
;     (let ((base-id (formal-id (assoc-formal (peer s)))))
;       (if (port-multi? (assoc-formal (peer s)))
; 	  (+ base-id (list-index (assoc-value (peer s)) s))
; 	  base-id)))
;      
;   (define (signal->csig s peer)
;     (list (primitive-comp-dyncomp (assoc-comp (peer s)))
; 	  (signal-id s peer)
; 	  (signal-read-speed s) (signal-write-speed s)))
;
;   (define (signal->csig-in s) (signal->csig s signal-src))
;   (define (signal->csig-out s) (signal->csig s signal-dst))
;
;   (define (comp->ccomp c)
;     (let loop ((in '())
; 	       (out '())
; 	       (ass (comp-assocs c)))
;       (if (null? ass)      
; 	  (list (primitive-comp-dyncomp c) in out)
; 	  (let ((f (assoc-formal (car ass)))
; 		(v (assoc-value (car ass))))
; 	    (if (port? f)
; 		(case (port-dir f)
; 		  ((in)
; 		   (loop (append! in (map signal->csig-in v))
; 			 out
; 			 (cdr ass)))
; 		  ((out)
; 		   (loop in
; 			 (append! out (map signal->csig-out v))
; 			 (cdr ass))))
; 		(loop in out (cdr ass)))))))
;
;   (map comp->ccomp (net-comps net)))

(define (connect-dyncomps net)

  (define (signal-buf-size sig)
    (apply max (map buf-size (map signal-head-buf (signal-heads sig)))))

  (define (signal-src-id s)
    ;; Handle multi-ports in the way that the C side expects them.
    (let ((base-id (formal-id (assoc-formal (signal-src s)))))
      (if (port-multi? (assoc-formal (signal-src s)))
	  (+ base-id (list-index (assoc-value (signal-src s)) s))
	  base-id)))

  (define (connect c)
    (let loop ((in '())
 	       (out '())
 	       (ass (comp-assocs c)))
      (cond
       ((null? ass)
	;;(pk (primitive-comp-dyncomp c) 'in in 'out out)
	(dyncomp-connect (primitive-comp-dyncomp c) in out))
       (else
	(let* ((a (car ass))
	       (f (assoc-formal a))
	       (v (assoc-value a)))
	  (define (signal-head->csig-in sh)
	    (if sh
		(let ((s (signal-head-sig sh)))
		  (list (primitive-comp-dyncomp (assoc-comp (signal-src s)))
			(signal-src-id s)
			(signal-buf-size s)
			(buf-delay (signal-head-buf sh))))
		(list #f 0 (* (box-ticks (comp-box c)) (assoc-dst-size a)) 0)))
	  (define (signal->csig-out s)
	    (list (if s
		      (signal-buf-size s)
		      (* (box-ticks (comp-box c)) (assoc-src-size a)))))
	  (if (port? f)
	      (case (port-dir f)
		((in)
    		 (loop (append! in (map signal-head->csig-in v))
		       out
		       (cdr ass)))
		((out)
		 (loop in
		       (append! out (map signal->csig-out v))
		       (cdr ass))))
	      (loop in out (cdr ass))))))))

  (for-each connect (net-comps net)))
	      
;; c-schedule: (stepper...)
;; stepper: (count thing)
;; thing: dyncomp
;;      | c-schedule

(define (print-csched csched indent)
  (define (spaces n) (make-string n #\space))
  (define (print-stepper st l)
    (cond ((list? (cadr st))
	   (format #t "~a~a~%" (spaces l) (car st))
	   (print-csched (cadr st) (1+ l)))
	  (else
	   (format #t "~a~a: ~a~%" (spaces l) (car st) (cadr st)))))
  (do-list st csched
    (print-stepper st indent)))


;;; XXX - make the next routines less recursive

;; When SCHED contains a stepper with count=1 which has another
;; schedule as its thing, splice that schedule into SCHED.
;;
(define (flatten-csched sched)
  (cond
   ((null? sched)
    sched)
   ((and (= (car (car sched)) 1) (list? (cadr (car sched))))
    (append! (flatten-csched (cadr (car sched)))
	     (flatten-csched (cdr sched))))
   ((list? (cadr (car sched)))
    (cons (list (car (car sched)) (flatten-csched (cadr (car sched))))
	  (flatten-csched (cdr sched))))
   (else
    (cons (car sched) (flatten-csched (cdr sched))))))

(define (list-derivative l)
  (let loop ((res '())
	     (n 0)
	     (l l))
    (cond ((null? l)
	   (reverse! res))
	  (else
	   (loop (cons (- (car l) n) res) (car l) (cdr l))))))

(define (sched->csched top-box)
  (define (buf-chunk-delay buf)
    ;; XXX - delay must be multiple of destination chunk size
    (if (not (zero? (remainder (buf-delay buf) (buf-dst-size buf))))
	(error "delay must be multiple of destination chunk size, sorry."))
    (/ (buf-delay buf) (buf-dst-size buf)))
  (define (find-primitive-stepper box)
    (let ((dc (primitive-comp-dyncomp (primitive-box-comp box)))
	  (steps (delete! 0
			  (list-derivative
			   (append!
			    (sort (map (lambda (buf)
					 (remainder (buf-chunk-delay buf)
						    (box-ticks box)))
				       (box-in box)) <)
			    (list (box-ticks box)))))))
      (list 1 (map (lambda (s) (list s dc)) steps))))
  (define (box->stepper box)
    (if (primitive-box? box)
	(find-primitive-stepper box)
	(list (box-ticks box)
	      (map box->stepper (sequence-box-sequence box)))))
  (flatten-csched (list (box->stepper top-box))))

(define (run-simulation net sched)
  (define (notify op)
    ;; Return `#f' to stop simulation.
    (case op 
      ((finished)
       (with-net net
         (if (comp-finished? net) #f #t)))
      (else 
       #f)))
  (let* ((csched (sched->csched sched)))
    (cond ((verbose?)
	   (format #t "Flattened schedule:~%")
	   (print-csched csched 1)
	   (format #t "~%")))
    (connect-dyncomps net)
    (gc)
    (dynsim-run (make-dynsim csched) notify)
    ;; primitive epilogs have already been run, now run hierarchical
    ;; epilogs
    (with-net net
       (comp-run-epilog net))))
