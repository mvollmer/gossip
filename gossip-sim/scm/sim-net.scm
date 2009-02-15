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

(read-set! keywords 'prefix)

(define-module (gossip sim-net)
  :use-module (oop goops)
  :use-module (gossip sim-util)
  :use-module (gossip sim-builtin)
  :use-module (gossip sim-options)
  :use-module (ice-9 format)
  :use-module (ice-9 optargs)
  :use-module (ice-9 rdelim))

(export sim-inspect)

;;; Overview

;; A BLOCK represents a specific data processing task.  It has a
;; description of the interface of the block, including GENERICS that
;; set parameters that are constant throughout a simulation, and PORTS
;; that can accept SIGNALS.
;;
;; A BLOCK needs to be instantiated to yield a COMPONENT, giving
;; specific values for the generics and connecting speficic signals to
;; the ports of the BLOCK.  Components can then be used to build up a
;; NET.  The net can finally be executed.
;;
;; Example: FIR filter
;;
;; The general algorithm for performing a FIR filter operation is
;; implemented in a BLOCK.  The GENERICS are the constant coefficients
;; of the filter.  The PORTS are the input and ouput for the signal.
;;
;; When instantiating the BLOCK into a COMPONENT, the coefficients get
;; specific values to effect a low-pass filter, for example.  The
;; PORTS are connected to signals that connect the filter to other
;; components that are part of the processing chain.


;;; Formals and interfaces

;; A FORMAL is either a GENERIC or a PORT.  Every FORMAL has a name
;; and a type.  The type is largely uninterpreted by the upper layers
;; of Gossip, but it is checked that only ports of compatible types
;; are connected.

(define-struct formal () block name type (id #f))
(define-struct generic (formal) default? default-value)
(define-struct port (formal) dir multi?)

;; A PREFORMAL is either a FORMAL as defined above or simply
;; a list in one of the forms
;;
;;    (= name type [[default?] default-value])
;;    (< name type)
;;    (> name type)
;;
;; It is used as a convenient read-syntax for formals and also in the
;; interface to primitive blocks.  The car of the list indicates
;; whether the formal is for a generic (=), input (<), or output (>)
;; port.  DEFAULT? is a boolean that specifies whether the generic can
;; be defaulted.  TYPE is either a symbol which denotes the type, or
;; (multi TYPE) which denotes a multi-port of type TYPE.

;; any unique value will do.
(define the-undefined-generic-value (list 'undefined-generic-value))

(define (make-formal block mode name type . rest)
  (case mode
    ((=)
     (let-optional rest ((default? #t) 
			 (default-value the-undefined-generic-value))
       (make generic
	 :block block :name name :type type
	 :default? default? :default-value default-value)))
    ((< >)
     (let ((multi #f))
       (if (and (list? type) (eq? (car type) 'multi))
	   (begin
	     (set! multi #t)
	     (set! type (cadr type))))
       (make port :block block :name name :type type
	     :dir (if (eq? mode '<) 'in 'out) :multi? multi)))))

(define (preformal->formal block pfml)
  (if (formal? pfml)
      (let ((fml (copy-formal pfml)))
	(set-formal-block! fml block)
	fml)
      (apply make-formal block pfml)))

(export preformal->formal)

(define type-compatible? equal?)

(define (instantiate b . kv-list)
  (catch 'inst-error
	 (lambda ()
	   (block-instantiate b kv-list))
	 (lambda args #f)))

(define-class instantiable-class (<operator-class>))

(define-method (initialize (i instantiable-class) args)
  (next-method i (append args (list #:procedure instantiate))))

(define-class instantiable () :metaclass instantiable-class)

(define-struct block (instantiable) name interface)

(define-generic block-instantiate)

(export instantiable instantiate block-instantiate)

;;; COMPONENTs

;; A COMPONENT represents one instance of a BLOCK.  Components are
;; inherently hierachical, thus they can have a parent and children.
;; The PARENT is either a component one level up, or `#f'.  CHILDREN
;; is the possibly empty list of children of this component one level
;; down.  A component also carries its level number.  This is used to
;; find connected components on the same level, see SIGNAL below.

;; The VALUE of an ASSOC is either a single value (for generics), or a
;; list of signals (for ports).  Note that non-multi ports also have a
;; list, but this list is guaranteed to contain exactly one signal.
;;
;; Ports can also be `terminated'.  In that case, their list of
;; signals include a `#f' for the terminated sub-port.  Terminated
;; output ports throw their data away, while terminated input ports
;; get a constant bit-wise zero.

(define-struct assoc () comp formal value)

(define-struct comp ()
  block assocs (parent #f) (children '()) (level 0)
  (finished-predicate #f) (name #f) (epilog #f) (results '())
  (box #f)) ; XXX - only needed for open ports

(define (comp-finished? c) (call-with-comp c (comp-finished-predicate c)))
(export comp-finished?)

(define (comp-run-epilog c)
  (for-each comp-run-epilog (comp-children c))
  (if (comp-epilog c) (call-with-comp c (comp-epilog c))))

(define (comp-set-result c res val)
  (set! (comp-results c) (acons res val (comp-results c))))

(export comp-run-epilog comp-set-result)

(define-method (initialize (c comp) initargs)
  (next-method)
  (let ((p (comp-parent c)))
    (cond
     (p
      (set! (comp-level c) (1+ (comp-level p)))
      (set! (comp-children p) (cons c (comp-children p)))))))

;; A SIGNAL together with one or more SIGNAL-HEADS connects ports on
;; the bottom level of a net.  It also has a name for debugging
;; purposes.
;;
;; The SRC of a signal points to the assoc that connects the signal to
;; its source port.
;;
;; The heads of a signal contain the per-connection information on the
;; destination side.  A signal can be connected to multiple
;; destinations with a different delay for each destination, for
;; example. The DST of a signal-head points to the assoc for the
;; destination port.
;;
;; A signal can also be a `forwarding' signal.  This is the case when
;; its SRC slot does not point to an assoc but to a signal-head.
;; Forwarding signals do not have signal-heads and are not considered
;; part of the net.  They arise from connecting two signals.  When a
;; signal-head is constructed for a forwarding signal, the
;; forwarded-to signal-head is used instead as the basis.
;;
;; No signal returned by NET-SIGS will be a forwarding signal.  The
;; effect of connecting signals is achieved by merging them into one
;; equivalent signal.
;;
;; A second complication are `terminated' signals.  A
;; output-terminating signal is one that has `#f' in its list of
;; signal-heads.  A input-terminating signal has `#t' as its
;; signal-src (it has `#f' as its signal-src when it is unconnected).
;; No terminating signal or signal-head is ever part of a net, they
;; are converted to terminated ports.

(define-struct signal () name (src #f) (heads '()))
(define-struct signal-head () sig (dst #f) (buf #f)
  (delay #f) (chunk-delay #f))

;; XXX - make the next two functions into slots?

(define (assoc-size a dynsizer)
  (let* ((f (assoc-formal a))
	 (c (assoc-comp a)))
    (dynsizer (primitive-comp-dyncomp c) (formal-id f))))

(define (assoc-src-size src) (assoc-size src dyncomp-out-size))
(define (assoc-dst-size dst) (assoc-size dst dyncomp-in-size))
(define (assoc-src-type-size src) (assoc-size src dyncomp-out-type-size))
(define (assoc-dst-type-size dst) (assoc-size dst dyncomp-in-type-size))

(define (make-signal . args)
  (let-optional args ((name #f))
    (let-keywords args #f ((bus #f))
      (if bus
	  (map (lambda (i)
		 (make signal :name (if name 
					(string-append (symbol->string name)
						       "."
						       (number->string i))
					#f)))
	       (iota bus))
	  (make signal :name name)))))

(define (make-signal-head sig . args)
  (cond 
   ((eq? #f sig)
    #f)
   ((list? sig)
    (map (lambda (s) (apply make-signal-head s args)) sig))
   ((and (signal? sig) (signal-head? (signal-src sig)))
    (apply make-signal-head (signal-src sig) args))
   (else
    (let-keywords args #f ((delay 0)
			   (chunk-delay 0))
      (if (< delay 0)
	  (error "delay must be non-negative"))
      (if (< chunk-delay 0)
	  (error "chunk-delay must be non-negative"))
      (cond
       ((signal-head? sig)
	(make signal-head
	  :sig (signal-head-sig sig)
	  :delay (+ (signal-head-delay sig) delay)
	  :chunk-delay (+ (signal-head-chunk-delay sig) chunk-delay)))
       (else
	(make signal-head
	  :sig sig
	  :delay delay
	  :chunk-delay chunk-delay)))))))

(define (canonicalize-signal sig)
  (cond ((null? sig)
	 '())
	((not (pair? sig))
	 (list sig))
	(else
	 (apply append! (map canonicalize-signal sig)))))

(define (connect-signals in out)
  (define (connect-error . msg-args)      ;; XXX - generalize
    (set! (net-errors (current-net)) #t)
    (sim-error (lambda (p) (eq? p connect-signals)) msg-args)
    #f)
  (for-each (lambda (in out)
	      (define (grab-signal-head sh)
		(cond (sh
		       (set! (signal-head-delay sh) 
			     (+ (signal-head-delay sh)
				(signal-head-delay in)))
		       (set! (signal-head-chunk-delay sh)
			     (+ (signal-head-chunk-delay sh)
				(signal-head-chunk-delay in)))
		       (set! (signal-head-sig sh) (signal-head-sig in))))
		(if (or (not sh) (signal-head-dst sh))
		    (set! (signal-heads (signal-head-sig in))
			  (cons sh (signal-heads (signal-head-sig in))))))
	      (cond ((eq? in out)
		     #t)
		    ((and out (not (signal? out)))
		     (connect-error "~a is not a signal" out))
		    ((and in (not (or (signal? in) (signal-head? in))))
		     (connect-error "~a is not a signal" in))
		    ((and out (signal-src out))
		     (connect-error "signal ~a already connected to source"
				    (signal-name out)))
		    ((eq? in #f)
		     (set! (signal-src out) #t))
		    ((or (signal? in) (signal-head-dst in))
		     (connect-signals (make-signal-head in) out))
		    ((eq? out #f)
		     (let ((insig (signal-head-sig in)))
		       (set! (signal-heads insig)
			     (cons #f (signal-heads insig)))))
		    (else
		     (set! (signal-src out) in)
		     (for-each grab-signal-head (signal-heads out))
		     (set! (signal-heads out) '())
		     (set! (net-sigs (current-net))
			   (delq out (net-sigs (current-net)))))))
	    (canonicalize-signal in)
	    (canonicalize-signal out)))

;; Attach SH to its signal, following inter-signal connections.

(define (connect-signal-head sh)
  (let* ((s (signal-head-sig sh))
	 (ss (signal-src s)))
    (cond ((signal-head? ss)
	   (set! (signal-head-delay sh) 
		 (+ (signal-head-delay sh)
		    (signal-head-delay ss)))
	   (set! (signal-head-chunk-delay sh)
		 (+ (signal-head-chunk-delay sh)
		    (signal-head-chunk-delay ss)))
;; 	   (pk 'from (signal-name (signal-head-sig sh))
;; 	       'to (signal-name (signal-head-sig ss)))
	   (set! (signal-head-sig sh) (signal-head-sig ss))
	   (connect-signal-head sh))
	  (else
	   (set! (signal-heads s) (cons sh (signal-heads s)))))))

(export assoc-src-size assoc-dst-size assoc-src-type-size assoc-dst-type-size
	make-signal make-signal-head connect-signals)

;;; NETs

;; A NET is a completely elaborated, hierachical collection of
;; components.  It can be viewed as the single level-0 component.
;;
;; In addition to the usual hierarchical featurs of a component that
;; can be used to walk the net, a net has also a list of all
;; bottom-level components, and a list of all signals.  This allows
;; easy access to the elaborated net.

(define-struct net (comp)
  (comps '()) (sigs '()) (seqno 1)
  (errors #f) (properties '()))

(define net-property
  (make-procedure-with-setter
   (lambda (n p . def)
     (let ((cell (assq p (net-properties n))))
       (cond (cell
	      (cdr cell))
	     ((null? def)
	      (error "no such property" p))
	     (else
	      (car def)))))
   (lambda (n p v)
     (let ((cell (assq p (net-properties n))))
       (cond (cell
	      (set-cdr! cell v))
	     (else
	      (set! (net-properties n) (acons p v (net-properties n)))))))))

(export net-property)

(define (make-net-property-accessor prop)
  (make-procedure-with-setter
   (lambda (n)
     (net-property n prop))
   (lambda (n v)
     (set! (net-property n prop) v))))

(export make-net-property-accessor)

;; Some standard net properties

(define net-default-error-prefix
  (make-net-property-accessor 'default-error-prefix))

(export net-default-error-prefix)

;; Return a new number that is unique among all BLOCKs in NET

(define (get-comp-seqno net block)
  ;; For now, just ignore BLOCK
  (let ((seqno (net-seqno net)))
    (set! (net-seqno net) (1+ seqno))
    seqno))

;; Add COMP to NET.  PARENT, LEVEL, CHILDREN and ASSOCS of COMP must
;; be already initialized.  CHILDREN must be added _before_ their
;; PARENTs.  This function takes care to initialize the SIGNALs.

(define (net-add net comp)

  (define (check-type sig dst)
    (let ((src (signal-src sig)))
      (if (and (assoc? src)
	       (not (type-compatible?
		     (formal-type (assoc-formal src))
		     (formal-type (assoc-formal dst)))))
	  (inst-error "type mismatch on signal ~a: ~a -> ~a"
		      (signal-name sig)
		      (formal-type (assoc-formal src))
		      (formal-type (assoc-formal dst))))))

  (cond
   ((null? (comp-children comp))
    (set! (net-comps net) (cons comp (net-comps net)))
    (do-list a (comp-assocs comp)
      (let ((f (assoc-formal a))
	    (v (assoc-value a)))
	(cond
	 ((port? f)
	  (case (port-dir f)
	    ((in)
	     (do-list sh v
               (cond (sh
		      (connect-signal-head sh)
		      (let ((s (signal-head-sig sh)))
			(if (not (memq s (net-sigs net)))
			    (set! (net-sigs net) (cons s (net-sigs net))))
			(set! (signal-head-dst sh) a)
			(check-type s a))))))
	    ((out)
	     (do-list s v
	       (cond (s
		      (if (not (memq s (net-sigs net)))
			  (set! (net-sigs net) (cons s (net-sigs net))))
		      (if (signal-src s)
			  (inst-error "signal ~a already connected to a source"
				      (signal-name s)))
		      (set! (signal-src s) a)
		      (for-each (lambda (sh)
				  (if sh
				      (check-type s (signal-head-dst sh))))
				(signal-heads s))))))))))))))

(define (net-finish net)

  (if (verbose?)
      (print net))

  (do-list s (net-sigs net)
    (if (not (signal-src s))
	(net-error net "signal ~a has no source" (signal-name s)))
    (if (null? (signal-heads s))
	(net-error net "signal ~a has no destinations"
		   (signal-name s)))
    
    ;; output termination
    (cond ((memq #f (signal-heads s))
	   (cond ((null? (delq #f (signal-heads s)))
		  ;; terminate port
		  (set! (net-sigs net) (delq s (net-sigs net)))
		  (set! (assoc-value (signal-src s))
			(map (lambda (v) (if (eq? s v) #f v))
			     (assoc-value (signal-src s)))))
		 (else
		  ;; drop termination
		  (set! (signal-heads s) (delq #f (signal-heads s)))))))

    ;; input termination
    (cond ((eq? (signal-src s) #t)
	   (do-list sh (signal-heads s)
	     (let ((a (signal-head-dst sh)))
	       (set! (assoc-value a)
		     (map (lambda (v) (if (eq? v sh) #f v))
			  (assoc-value a)))))
	   (set! (net-sigs net) (delq s (net-sigs net)))))))


;; A net is normally constructed by creating an empty NET structure,
;; making it the `current net' and then instantiatin blocks in it by
;; invoking the block objects as functions (which in turn leads to a
;; call to BLOCK-INSTANTIATE).  At any one time, there is also a current
;; component that is in the process of being elaborated.

(define the-current-net-fluid (make-fluid))
(define the-current-comp-fluid (make-fluid))

(define (current-net) (fluid-ref the-current-net-fluid))
(define (current-comp) (fluid-ref the-current-comp-fluid))

(define (call-with-net net thunk)
  (with-fluids ((the-current-net-fluid net)
		(the-current-comp-fluid net))
	       (thunk)))

(define-macro (with-net net . body)
  `(call-with-net ,net (lambda () ,@body)))

(define (call-with-comp comp thunk)
  (with-fluids ((the-current-comp-fluid comp))
	       (thunk)))

(define-macro (with-comp comp . body)
  `(call-with-comp ,comp (lambda () ,@body)))

(define (make-default-hierarchical-finished-predicate c)
  (lambda ()
    (or-map comp-finished? (comp-children c))))

(define inspect-on-error #f)

(define (make-net setup default-error-prefix)
  (let ((net (make net :name "Net" :block #f :assocs '())))
    (set! (comp-finished-predicate net) 
	  (make-default-hierarchical-finished-predicate net))
    (set! (net-default-error-prefix net) default-error-prefix)
    (call-with-net net setup)
    (net-finish net)
    (cond
     ((net-errors net)
      (if inspect-on-error
	  (sim-inspect net))
      #f)
     (else
      net))))

(define (inst-error . msg-args)
  (set! (net-errors (current-net)) #t)
  (sim-error (lambda (p) (is-a? p instantiable)) msg-args)
  (throw 'inst-error))

(define (net-error net . msg-args)
  (set! (net-errors net) #t)
  (let ((prefix (or (and (current-load-port)
			 (port-filename (current-load-port)))
		    (net-default-error-prefix net)
		    "gossip")))
    (format #t "~a: " prefix)
    (apply format #t msg-args)
    (newline)))

(export current-net current-comp call-with-net with-net call-with-comp 
	with-comp make-net inspect-on-errors)

;; This function builds a canonical list of ASSOCS from a list of
;; keyword value pairs.  The assocs will contain the settings for
;; every generic (substituting the default value for a missing one)
;; and signals for multi-ports are collected into one assoc.  This
;; function also checks for error situations so that the resulting
;; ASSOC list is guarenteed to be error free.

(define (make-assocs comp kv-list)
  
  (define (get-vals sym)
    (let loop ((res '())
	       (kv-list kv-list))
      (cond
       ((null? kv-list)
	(reverse! res))
       ((or (not (keyword? (car kv-list)))
	    (null? (cdr kv-list)))
	(inst-error "malformed keyword/value list: ~a" kv-list))
       (else
	(if (eq? sym (keyword->symbol (car kv-list)))
	    (loop (cons (cadr kv-list) res) (cddr kv-list))
	    (loop res (cddr kv-list)))))))
  
  (let loop ((assocs '())
	     (formals (block-interface (comp-block comp))))
    (cond
     ((null? formals)
      (reverse! assocs))
     (else
      (let* ((f (car formals))
	     (v (get-vals (formal-name f))))
	(cond
	 ((generic? f)
	  (cond
	   ((null? v)
	    (if (generic-default? f)
		(set! v (generic-default-value f))
		(inst-error "no value for generic `~a'" (formal-name f))))
	   ((null? (cdr v))
	    (set! v (car v)))
	   (else
	    (inst-error "to many values for generic `~a'" (formal-name f)))))
	 ((port? f)
	  (set! v (canonicalize-signal v))
	  (if (not (port-multi? f))
	      (cond ((null? v)
		     (inst-error "no signal connected to ~a" (formal-name f)))
		    ((not (null? (cdr v)))
		     (inst-error "too many signals connected to ~a"
				 (formal-name f)))))
	  (case (port-dir f)
	    ((in)
	     (set! v (map (lambda (s)
			    (cond
			     ((eq? s #f)
			      #f)
			     ((signal? s)
			      (make-signal-head s))
			     ((signal-head? s)
			      (if (signal-head-dst s)
				  (make-signal-head s)
				  s))
			     (else
			      (inst-error "not a signal: ~a" s))))
			  v)))
	    ((out)
	     (set! v (map (lambda (s)
			    (cond
			     ((not s)
			      ;; termination, construct dummy signal
			      (let ((ss (make-signal 'dummy)))
				(connect-signals ss #f)
				ss))
			     ((signal? s)
			      s)
			     (else
			      (inst-error "not a signal: ~a" s))))
			  v))))))
	(loop (cons (make assoc :comp comp :formal f :value v) assocs)
	      (cdr formals)))))))

(define (initialize-comp-misc c kv-list)
  (let-keywords kv-list #t ((name #f))
    (if name
	(set! (comp-name c) name)
	(set! (comp-name c) 
	      (string-append 
	       (block-name (comp-block c)) "."
	       (number->string 
		(get-comp-seqno (current-net) (comp-block c))))))))

;;; Concrete BLOCKs and COMPONENTs

;; A PRIMITIVE BLOCK is one that is implemented by dynamically loaded
;; C++ code.  It is constructed from the filename of the C++ shared
;; library that implements the block.

(define-struct primitive-block (block) dynblock)

(define (identify-formals b fs)
  (let loop ((fs fs)
	     (gen-id 0)
	     (in-id 0)
	     (out-id 0))
    (if (not (null? fs))
	(let ((f (car fs)))
	  (cond
	   ((generic? f)
	    (set! (formal-id f) gen-id)
	    (loop (cdr fs) (1+ gen-id) in-id out-id))
	   ((eq? (port-dir f) 'in)
	    (set! (formal-id f) in-id)
	    (loop (cdr fs) gen-id (1+ in-id) out-id))
	   (else
	    (set! (formal-id f) out-id)
	    (loop (cdr fs) gen-id in-id (1+ out-id))))))))

(define (make-primitive-block name filename)
  (define (read-interface block dynblock)
    (let ((str (dynblock-prototype dynblock)))
      (if str
	  (let ((p (call-with-input-string str read)))
	    (if (not (and (list? p) (eq? (car p) 'gossip-sim)))
		(error "not a gossip-sim block" filename))
	    (map (lambda (pf) (preformal->formal block pf)) (caddr p)))
	  (error "can't read interface" filename))))
  (let* ((db (dynblock-load filename))
	 (b (make primitive-block :name name :dynblock db)))
    (set! (block-interface b) (read-interface b db))
    (identify-formals b (block-interface b))
    b))

(define-struct primitive-comp (comp) dyncomp)

(define (count-connections as dir)
  (let loop ((n 0) (a as))
    (cond ((null? a) n)
	  (else
	   (let ((f (assoc-formal (car a))))
	     (if (and (port? f) (eq? (port-dir f) dir))
		 (loop (+ n (length (assoc-value (car a)))) (cdr a))
		 (loop n (cdr a))))))))

(define-method (block-instantiate (b primitive-block) kv-list)
  (let* ((c (let ((c (make primitive-comp
		       :block b
		       :parent (current-comp))))
	      (initialize-comp-misc c kv-list)
	      c))
	 (as (make-assocs c kv-list))
	 (dc (dynblock-instantiate (primitive-block-dynblock b)
				   (comp-name c)))
	 (gvals (let loop ((g '())
			   (a as))
		  (cond
		   ((null? a)
		    (reverse! g))
		   ((generic? (assoc-formal (car a)))
		    (loop (cons (assoc-value (car a)) g)
			  (cdr a)))
		   (else
		    (loop g (cdr a))))))
	 (n-in (count-connections as 'in))
	 (n-out (count-connections as 'out)))
    (dyncomp-init dc gvals the-undefined-generic-value n-in n-out)
    (set! (comp-assocs c) as)
    (set! (primitive-comp-dyncomp c) dc)
    (set! (comp-finished-predicate c) (lambda () (dyncomp-finished? dc)))
    (net-add (current-net) c)
    c))

(define (comp-result c res)
  (cond
   ((primitive-comp? c)
    (dyncomp-result (primitive-comp-dyncomp c) (cond
						((symbol? res)
						 (symbol->string res))
						((keyword? res)
						 (symbol->string
						  (keyword->symbol res)))
						(else
						 res))))
   (else
    (let ((cell (assq res (comp-results c))))
      (if cell (cdr cell) (error "no such result:" res))))))

(export make-primitive-block comp-result)

;; A hierarchical-block is a block that is instantiated by executing a
;; user supplied function.  That function is known as the `instfunc'
;; and is supposed to instantiate the nested blocks.

;; The instfunc gets one argument for every formal of the block, in
;; the order of the formals in the interface list.

(define-struct hierarchical-block (block) instfunc)

(define (make-hierarchical-block name interface instfunc)
  (let ((b (make hierarchical-block :name name :instfunc instfunc)))
    (set! (block-interface b) (map (lambda (f)
				     (preformal->formal b f))
				   interface))
    b))

(export make-hierarchical-block)

(define-method (block-instantiate (b hierarchical-block) kv-list)
  (let* ((c (make comp
	      :block b
	      :parent (current-comp)))
	 (as (make-assocs c kv-list)))
    (set! (comp-assocs c) as)
    (initialize-comp-misc c kv-list)
    (set! (comp-finished-predicate c)
	  (make-default-hierarchical-finished-predicate c))
    (with-comp c
	       (apply (hierarchical-block-instfunc b) (map assoc-value as)))
    (net-add (current-net) c)
    c))


;;; printing

(define-generic print)
(export print)

(define (spaces n) (make-string n #\space))

(define-method (print (c comp))
  (let ((spc (spaces (* 2 (comp-level c)))))
    (format #t "~a~a:~%" spc (comp-name c))
    (do-list a (comp-assocs c)
      (let ((f (assoc-formal a))
	    (v (assoc-value a)))
	(format #t "~a  ~a " spc (formal-name f))
	(if (generic? f)
	    (format #t "= ~a~%" (if (eq? v the-undefined-generic-value)
				    "(default)" v))
	    (cond
	     ((eq? (port-dir f) 'in)
	      (format #t "<")
	      (do-list sh v
		(if sh
		    (let ((src (signal-src (signal-head-sig sh))))
		      (cond
		       ((not src)
			(format #t " (unconnected signal ~a)" 
				(signal-name (signal-head-sig sh))))
		       ((eq? src #t)
			(format #t " (open)"))
		       (else
			(format #t
				" ~a:~a" 
				(comp-name (assoc-comp src))
				(formal-name (assoc-formal src))))))
		    (format #t " (open)")))
	      (format #t "~%"))
	     ((eq? (port-dir f) 'out)
	      (format #t ">")
	      (do-list s v
		(format #t " ")
		(if s
		    (if (null? (signal-heads s))
			(format #t "(unconnected signal ~a)" (signal-name s))
			(let loop ((heads (signal-heads s)))
			  (cond ((not (null? heads))
				 (if (car heads)
				     (let ((d (signal-head-dst (car heads))))
				       (format #t "~a:~a"
					       (comp-name (assoc-comp d))
					       (formal-name (assoc-formal d))))
				     (format #t "(open)"))
				 (if (not (null? (cdr heads)))
				     (format #t ","))
				 (loop (cdr heads))))))
		    (format #t "(open)")))
	      (format #t "~%"))))))
    (do-list sc (reverse (comp-children c))
      (print sc))))

;;; Inspection

(define (@ . args) (apply format #t args))

;; Print the object and return a list with referenced objects.
(define-generic print-for-inspection)

(define (read-with-prompt prompt)
  (display prompt (current-output-port))
  (force-output (current-output-port))
  (read-line))

(define (sim-inspect obj)
  (define (inspect obj history)
    (let ((refs (print-for-inspection obj)))
      (let loop ()
	(let ((choice (read-with-prompt "> ")))
	  (cond
	   ((eof-object? choice)
	    #f)
	   ((string=? choice "?")
	    (inspect obj history))
	   ((string=? choice "q")
	    #t)
	   ((string=? choice ".")
	    (cond ((null? history)
		   (@ "Nothing to go back to.~%")
		   (loop))
		  (else
		   (inspect (car history) (cdr history)))))
	   ((string->number choice)
	    => (lambda (n)
		 (cond
		  ((and (>= n 0) (< n (length refs)))
		   (inspect (list-ref refs n) (cons obj history)))
		  (else
		   (@ "No reference ~a.~%" n)
		   (loop)))))
	   (else
	    (@ "'?' prints the object again. 'q' quits. '.' goes back.~%")
	    (@ "Numbers follow the indicated references.~%")
	    (loop)))))))
  (inspect obj '()))

(define-method (print-for-inspection (n net))
  (@ "Net: ~a components, ~a signals.~%"
     (length (net-comps n)) (length (net-sigs n)))
  (@ "properties: ~a~%" (net-properties n))
  (print-component n 0))

(define-method (print-for-inspection (c comp))
  (@ "Component ~a:~%" (comp-name c))
  (print-component c 0))

(define (print-component c start)
  (define (print-assocs start)
    (let loop ((assocs (comp-assocs c))
	       (refs '())
	       (i start))
      (cond
       ((null? assocs)
	(reverse! refs))
       (else
	(let* ((a (car assocs))
	       (f (assoc-formal a))
	       (v (assoc-value a)))
	  (@ " ~a " (formal-name f))
	  (cond
	   ((generic? f)
	    (@ "= ~a~%" (if (eq? v the-undefined-generic-value)
			    "(default)" v))
	    (loop (cdr assocs) refs i))
	   ((eq? (port-dir f) 'in)
	    (@ "<")
	    (let head-loop ((sheads v)
			    (refs refs)
			    (i i))
	      (cond
	       ((null? sheads)
		(@ "~%")
		(loop (cdr assocs) refs i))
	       ((car sheads)
		(@ " ~a[~a]" (signal-name (signal-head-sig (car sheads))) i)
		(head-loop (cdr sheads) 
			   (cons (signal-head-sig (car sheads)) refs)
			   (1+ i)))
	       (else
		(@ " (open)")
		(head-loop (cdr sheads) refs i)))))
	   ((eq? (port-dir f) 'out)
	    (@ ">")
	    (let sig-loop ((sigs v)
			   (refs refs)
			   (i i))
	      (cond
	       ((null? sigs)
		(@ "~%")
		(loop (cdr assocs) refs i))
	       ((car sigs)
		(@ " ~a[~a]" (signal-name (car sigs)) i)
		(sig-loop (cdr sigs) 
			  (cons (car sigs) refs)
			  (1+ i)))
	       (else
		(@ " (open)")
		(sig-loop (cdr sigs) refs i)))))))))))

  (define (print-children start)
    (if (not (null? (comp-children c)))
	(@ "children:~%"))
    (let loop ((childs (comp-children c))
	       (refs '())
	       (i start))
      (cond 
       ((null? childs)
	(reverse! refs))
       (else
	(@ " ~a[~a]~%" (comp-name (car childs)) i)
	(loop (cdr childs) (cons (car childs) refs) (1+ i))))))

  (let* ((assoc-refs (print-assocs start))
	 (children-refs (print-children (+ start (length assoc-refs)))))
    (append! assoc-refs children-refs)))

(define-method (print-for-inspection (s signal))
  (@ "Signal ~a:~%" (signal-name s))
  (let ((src (signal-src s)))
    (@ " source: ~a:~a[0]~%"
       (comp-name (assoc-comp src))
       (formal-name (assoc-formal src)))
    (@ " destinations:")
    (let loop ((heads (signal-heads s))
	       (refs (list (assoc-comp src)))
	       (i 1))
      (cond
       ((null? heads)
	(reverse! refs))
       (else
	(let ((dst (signal-head-dst (car heads))))
	  (@ " ~a:~a[~a]~%"
	     (comp-name (assoc-comp dst))
	     (formal-name (assoc-formal dst))
	     i)
	  (loop (cdr heads) (cons (assoc-comp dst) refs) (1+ i))))))))

