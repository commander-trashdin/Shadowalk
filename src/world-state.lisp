;; This work is licensed under the Creative Commons Attribution 3.0 Unported License.
;; To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/
;; or send a letter to Creative Commons,
;;      PO Box 1866, Mountain View, CA 94042, USA.Copyright (C) 2003-2008 Shawn Betts
;;
;; This file is part of rpg.
;;
;; rpg is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.


;; Commentary:
;;
;; This file contains world state globals.
;;
;; Code:


(in-package #:rpg)

;; *system-place* describes project directory
;; *session-log* should be the current session log file -- TODO
;; *races* describes the current set of available races, should be read from a file
;; *creatures* describe ...something, for now its just all the existing creatures -- TODO
;; *items* describe existing items -- should be read from a file
;; *actions* describe existing actions
;; *skills* describe availiable skills, semi-fixed, since new skills are not added during the game. I read them from file as of now -- TODO

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *system-place* (asdf:system-source-directory 'rpg))
  (defparameter *session-log* (concatenate 'string (directory-namestring *system-place*) "log/session-log.lisp"))
  (defparameter *races* (%read-log (concatenate 'string (directory-namestring *system-place*) "log/races.log")))
  (defparameter *creatures* (make-hash-table :test 'eq))
  (defparameter *items* (make-hash-table :test 'eq))
  (defparameter *actions* (make-hash-table :test 'eq))
  (defparameter *skills* (%read-log (concatenate 'string (directory-namestring *system-place*) "/log/skills.log"))))
