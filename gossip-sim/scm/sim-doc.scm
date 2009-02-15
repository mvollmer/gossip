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

(define-module (gossip sim-doc)
  :use-module (ice-9 and-let-star)
  :use-module (gossip sim-config)
  :use-module (gossip sim-library))

(export sim-browse-doc)

(define (browse url)
  (simple-format #t "Browsing to ~s~%" url)
  (let ((rc (system (simple-format #f "netscape -remote 'OpenURL(~a)'" url))))
    (if (not (zero? rc))
	(system (simple-format #f "netscape '~a' &" url)))))

(define (sim-browse-doc lib block)
  (and-let* ((file (find-library-file lib "html.index"))
	     (index (with-input-from-file file read))
	     (entry (assq (string->symbol block) index)))
	(browse (cdr entry))
	#t))
