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


;;;-------------------------------------------------------------------
;;; Interface
;;;-------------------------------------------------------------------



(defmacro %defcharacter (name race &rest info)
  (type-safe (race-error) (assert (typep race 'races) nil
                                  'race-error :wrong-race race))
  (let ((%const 0) (%size 0) (%react 0) (%int 0))
    (type-safe (malformed-creation)
               `(progn
                  (let ((this (make-instance 'creature :name (%sym-to-str ',name) :race ',race)))
                    ,@(loop :for (slot . stats) :in info
                            :collect
                            (case slot
                              (stats
                               (assert (typep stats '(cons (%fixed-list fixnum 9) null)) nil
                                       'malformed-creation :place (car stats))
                               (destructuring-bind (str agi const chr per int will logic react) (car stats)
                                 (setf %const const %react react %int int)
                                 `(setf (strength this) ,str (agility this) ,agi (constitution this) ,const (charisma this) ,chr
                                        (perception this) ,per (intuition this) ,int (will this) ,will
                                        (logic this) ,logic (reaction this) ,react)))
                              (size
                               (assert (typep stats '(%fixed-list fixnum 1)) nil
                                       'malformed-creation :place (car stats))
                               (setf %size (car stats)) `(setf (size this) ,(car stats)))
                              (skills
                               (assert (typep stats '(cons list null)) nil 'malformed-creation :place (car stats))
                               `(progn
                                  ,@(loop :for skill :in *skills*
                                          :for cur := (getf (car stats) (symbol->keyword skill))
                                          :when cur
                                            :collect `(setf (gethash ',skill (skill-list this)) ,cur))))
                              (otherwise `(setf (,slot this) ,(coerce (car stats) 'vector)))))
                    (setf (max-health this) ,(+ 9 %const (* 4 %size)))
                    (setf (cur-health this) ,(+ 9 %const (* 4 %size)))
                    (setf (initiative this) ,(+ %react %int))
                    (setf (gethash ',name *creatures*) this))))))


(defun %make-action (name creature &key (modifier 0) body)
  (declare (optimize (safety 3) (debug 3)))
  (type-safe (malformed-action entity-not-found)
             (progn
               (assert (gethash creature *creatures*) nil 'entity-not-found :entity (%sym-to-str creature))
               (if body
                   (progn
                     (if (gethash name *actions*)
                         (warn "An action ~s is being redefined" (%sym-to-str name))
                         (warn "A new action ~s is being defined" (%sym-to-str name)))
                     (progn
                       (assert (typep body '(%tuple fixnum symbol skill fixnum)) nil
                               'malformed-action :place "Incorrect implementation")
                       (destructuring-bind (dice stat skill check) body
                         (let ((definition
                                 `(defun ,name (creature &optional modifier)
                                    (declare (optimize (safety 3) (debug 3)))
                                    (%check-success (%roll :d ,dice :times (+ (,stat creature)
                                                                              (get-skill creature :skill-name ',skill))
                                                           :modifiers modifier) ,check)))
                               (ftype-decl
                                 `(declaim (ftype (function (creature &optional fixnum) fixnum) ,name)))
                               (action-add
                                 `(setf  (gethash ',name *actions*) ,(%sym-to-str name)))
                               (action-call
                                 `(,name ,(gethash creature *creatures*) ,modifier)))
                           (values
                            `(progn
                               ,definition
                               ,ftype-decl
                               ,action-add
                               ,action-call)
                            definition
                            ftype-decl
                            action-add
                            action-call)))))
                   (progn
                     (assert (gethash name *actions*) nil 'malformed-action :place "It does not exist!")
                     `(,name ,(gethash creature *creatures*) ,modifier))))))


(defmacro defcharacter (name race &rest info)
  `(prog1
       (%defcharacter ,name ,race ,@info)
     (add-to-log (format nil "~%;;A new character ~a of ~a~p is born"
                         (%sym-to-str ',name) (%sym-to-str ',race) 10))
     (add-to-log '(%defcharacter ,name ,race ,@info))))


;;; okay now this kinda works

(defmacro make-action (name creature &key (modifier 0) body)
  (multiple-value-bind (evaluation definition ftype-decl action-add)
      (%make-action name creature :modifier modifier :body body)
    `(let ((res ,evaluation))
       (add-to-log ,(format nil "A new action ~s~%~s~%~s was defined" definition ftype-decl action-add))
       (add-to-log (format nil "~%;;~a performs an action: ~a with result(s): ~s~%"
                           ,(%sym-to-str creature) ,(%sym-to-str name) res))
       (add-to-log '(%make-action ,name ,creature :modifier ,modifier :body ,body))
       res)))
