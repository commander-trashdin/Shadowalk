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
;; This file contains definitions of all entities.
;;
;; Code:


(in-package #:rpg)

(deftype races () `(member ,@*races*))

(deftype skill () `(member ,@*skills*))

(%defclass feat ()
  (declare (optimize (safety 3) (debug 3)))
  ((parameters
    :type list
    :initform nil)
   (action-tags
    :type list
    :initform nil)
   (skill-tags
    :type list
    :initform nil)))


(defclass trait (feat)
  ())

(defclass drawback (feat)
  ())

(%defclass item ()
  (declare (optimize (safety 3) (debug 3)))
  ((item-size
    :type fixnum
    :initform 0)
   (weight
    :type fixnum
    :initform 0)
   (worth
    :type fixnum
    :initform 0)))

(%defclass weapon (item)
  (declare (optimize (safety 3) (debug 3)))
  ((melee)
   (damage)
   (crit-dmg)
   (crit-threshold)
   (reach)
   (damage-type)
   (attributes-roll)
   (fatigue-damage)
   (fatigue-cost)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (%defclass creature ()
    (declare (optimize (safety 3) (debug 3)))
    ((name
      :type string
      :initarg :name)
     (race
      :type races
      :initarg :race)
     (age
      :type (integer 0)
      :initarg :age)
     (gender
      :type symbol
      :initarg :gender)
     (size
      :type (integer -2 5)
      :initform 0)
     (strength
      :type (integer 1 100)
      :initarg :str
      :initform 1)
     (agility
      :type (integer 1 100)
      :initarg :agi
      :initform 1)
     (constitution
      :type (integer 1 100)
      :initarg :const
      :initform 1)
     (charisma
      :type (integer 1 100)
      :initarg :chr
      :initform 1)
     (perception
      :type (integer 1 100)
      :initarg :per
      :initform 1)
     (intuition
      :type (integer 1 100)
      :initarg :int
      :initform 1)
     (will
      :type (integer 1 100)
      :initarg :will
      :initform 1)
     (logic
      :type (integer 1 100)
      :initarg :logic
      :initform 1)
     (reaction
      :type (integer 1 100)
      :initarg :react
      :initform 1)
     (fatigue
      :type (integer 0 200)
      :initarg :fat
      :initform 0)
     (max-health
      :type (integer 1 1000)
      :initarg :max-health
      :initform 1)
     (cur-health
      :type (integer -100 1000)
      :initarg :cur-health
      :initform 1)
     (initiative
      :type (integer 0 100)
      :initform 0)
     (status-effects
      :type hash-table
      :initform (make-hash-table :test 'eq))
     (magic-levels
      :type list
      :initform '(:fire 0 :water 0 :air 0 :earth 0 :astral 0 :death 0 :nature 0 :blood 0 :holy 0))
     (skill-list
      :type hash-table
      :initform (%skill-list))
     (feat-list
      :type (vector feat *)
      :initform (make-array 1 :element-type 'feat
                            :initial-element (make-instance 'feat)
                            :adjustable t :fill-pointer 0))
     (inventory
      :type (vector item *)
      :initform (make-array 10 :element-type 'item
                            :initial-element (make-instance 'item)
                            :adjustable t :fill-pointer 0)))))


(defmethod print-object ((this creature) stream)
  (print-unreadable-object (this stream :type t)
    (princ (name this) stream)
    (format stream ", ")
    (princ (%sym-to-str (race this)) stream)))
