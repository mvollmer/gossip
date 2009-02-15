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

(define-module (gossip sim-builtin))

(dynamic-call "gossip_init" (dynamic-link "libguilegossip"))

(export get-signal-outlet add-signal-notify
	dynblock? dynblock-load dynblock-prototype dynblock-instantiate
	dyncomp? dyncomp-dynblock dyncomp-init dyncomp-connect
	dyncomp-in-size dyncomp-out-size
	dyncomp-in-type-size dyncomp-out-type-size
	dyncomp-finished? dyncomp-result
	dynsim? make-dynsim dynsim-run)
