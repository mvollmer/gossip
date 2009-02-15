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

(define-module (gossip sim-options)
  :use-module (gossip sim-config))

(export set-verbose verbose?)
(export default-library-path)

(define verbose-flag #f)

(define (verbose?) verbose-flag)
(define (set-verbose f) (set! verbose-flag f))

(define (split-colon-path str)
  (if (or (not str) (zero? (string-length str)))
      '()
      (let ((pos (string-index str #\:)))
	(if pos
	    (cons (substring str 0 pos)
		  (split-colon-path (substring str (1+ pos))))
	    (list str)))))

(define (default-library-path)
  (let* ((env-prefix (getenv "GOSSIP_SIM_PREFIX"))
	 (prefix (or env-prefix sim-prefix))
	 (exec-prefix (if env-prefix
			  (or (getenv "GOSSIP_SIM_EXEC_PREFIX") env-prefix)
			  sim-exec-prefix))
	 (def (list (string-append prefix "/share/gossip/sim")
		    (string-append exec-prefix "/libexec/gossip/sim")))
	 (env (getenv "GOSSIP_SIM_LIBRARY_PATH")))
    (if env
	(append! (split-colon-path env) def)
	def)))
