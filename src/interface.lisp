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
;; This file contains core interface including functions and macros for
;; creating characters, actions, skills, etc.
;;
;; Code:


(in-package #:rpg)

(defmacro defcharacter (name race &rest info)
  (type-safe (race-error) (assert (typep race 'races) nil
                                  'race-error :wrong-race race))
  `(progn
     (let ((this (make-instance 'creature :name (%sym-to-str ',name) :race ',race)))
       ,@(loop :for (slot . (stats)) :in info
               :collect
               (case slot
                 (stats (type-safe (malformed-creation)
                                   (assert (typep stats '(%fixed-list fixnum 9)) nil 'malformed-creation :place stats))
                  `(setf (values (get-str this) (get-agi this) (get-const this) (get-chr this) (get-per this)
                                 (get-int this) (get-will this) (get-logic this) (get-react this))
                         (values ,@stats)))
                 (size `(setf (size this) ,stats))
                 (skills `(loop :for skill-value :in stats
                                :for skill :in *skills*
                                :do (setf (gethash skill (get-skill-list this) skill-value))))
                 (otherwise `(,slot ,(coerce stats 'vector) this))))
       (max-health (+ 9 (get-const this) (* 4 (size this))) this)
       (cur-health (get-max-health this) this)
       (%init (+ (get-react this) (get-int this)) this)
       (setf (gethash ',name *creatures*) this))))



(defmethod print-object ((this creature) stream)
  (print-unreadable-object (this stream :type t)
    (princ (get-name this) stream)
    (format stream ", ")
    (princ (%sym-to-str (get-race this)) stream)))


(defmacro make-action (name creature &body body)
  (if (gethash name *actions*)
      `(,name ,(gethash creature *creatures*))
      (progn
        (warn "A new action ~s is being defined" (%sym-to-str name))
        `(progn
           (defun ,name (creature)
             ,@body)
           (setf  (gethash ',name *actions*) (%sym-to-str ',name))
           (,name ,(gethash creature *creatures*))))))
