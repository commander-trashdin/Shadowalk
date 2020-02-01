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
;; This file contains utility functions and macros for internal use only.
;;
;; Code:


(in-package #:rpg)




(deftype %fixed-list (type length)
  `(cons ,type ,(if (= length 1) `null `(%fixed-list ,type ,(1- length)))))



(declaim (ftype (function (symbol) simple-string) %sym-to-str))
(defun %sym-to-str (name)
  (declare (optimize (safety 3) (debug 3)))
  (string-capitalize (symbol-name name)))


(declaim (ftype (function (&key (:d fixnum) (:times fixnum)
                                (:modifiers (or list fixnum))) (or fixnum list)) %roll))
(defun %roll (&key (d 6) (times 1) modifiers)
  (declare (optimize (safety 3) (debug 3)))
  (typecase modifiers
    ((and list (not null))
     (assert (typep modifiers `(%fixed-list fixnum ,times))
             nil 'type-error :expected-type `(%fixed-list fixnum ,times) :datum `modifiers)
     (if (/= times 1)
         (loop :for mod :in modifiers
               :collect (+ mod 1 (random d)))
         (+ (car modifiers) 1 (random d))))
    (otherwise
     (loop :repeat times :collect (+ 1 (random d) (if modifiers modifiers 0))))))



(declaim (ftype (function ((and list (not null)) fixnum) fixnum) check-success))
(defun %check-success (list border)
  (declare (optimize (safety 3) (debug 3)))
  (loop :for d :of-type fixnum :in list
        :count (>= d border)))

(defun %print-status-effect (key value stream)
  (declare (optimize (safety 3) (debug 3)))
  (format stream "~s, time left ~s~%" key value))

(defun %skill-list ()
  (declare (optimize (safety 3) (debug 3)))
  (let ((skill-list (make-hash-table :test 'eq)))
    (loop :for skill :in *skills*
          :do (setf (gethash skill skill-list) 0))
   skill-list))

(defun %display (this stream)
  (format stream "Name: ~a~%" (get-name this))
  (format stream "Race:~a, size ~s~%" (%sym-to-str (get-race this)) (size this))
  (format stream "Combat stats: Strength ~s, Agility ~s, Constitution ~s~%" (get-str this) (get-agi this) (get-const this))
  (format stream "Cunning stats: Charisma ~s, Intuition ~s, Perception ~s~%" (get-chr this) (get-int this) (get-per this))
  (format stream "Intelligence stats: Will ~s, Logic ~s, Reaction ~s~%" (get-will this) (get-logic this) (get-react this))
  (format stream "Health: ~s/~s~%" (get-cur-health this) (get-max-health this))
  (format stream "Fatigue: ~s~%" (get-fat this))
  (format stream "Initiative:~s~%" (get-init this))
  (maphash (lambda (key value) (%print-status-effect key value stream)) (status this))
  (loop :initially (format stream "Magic levels:~%")
        :for stuff :in (magic-levels this)
        :if (keywordp stuff)
          :do (format stream "  ~a:" (%sym-to-str stuff))
        :else
          :do (format stream "~s~%" stuff))
  (loop :initially (format stream "Skills:~%")
        :for i :from 1 :to 32
        :for skill being each hash-key of (get-skill-list this)
          :using (hash-value v)
        :do (format stream "~a: ~s, " (%sym-to-str skill) v)
        :when (zerop (rem i 8))
          :do (format stream "~%"))
  (loop :initially (format stream "Feats:~%")
        :for feat :across (get-feat-list this)
        :do (format stream "~a, " feat)
        :finally (format stream "~%"))
  (loop :initially (format stream "Traits:~%")
        :for trait :across (get-trait-list this)
        :do (format stream "~a, " trait)
        :finally (format stream "~%"))
  (loop :initially (format stream "Drawbacks:~%")
        :for db :across (get-drawback-list this)
        :do (format stream "~a, " db)
        :finally (format stream "~%"))
  (loop :initially (format stream "Inventory:~%")
        :for item :across (get-inv this)
        :do (format stream "~a, " item)
        :finally (format stream "~%")))

(defun %read-log (address)
  (with-open-file (stream address :direction :input :if-does-not-exist nil)
    (read stream)))

(declaim (ftype (function (creature &key (:skill-name symbol)) fixnum) get-skill))
(defun get-skill (creature &key skill-name)
  (declare (optimize (safety 3) (debug 3)))
  (gethash skill-name (get-skill-list creature)))




(define-condition race-error (type-error)
  ((wrong-race
     :type (and symbol (not race))
     :initarg :wrong-race
     :reader wrong-race
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The race ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-race condition))))))

(define-condition skill-error (type-error)
  ((wrong-skill
     :type (and symbol (not skill))
     :initarg :wrong-skill
     :reader wrong-skill
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The skill ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-skill condition))))))

(define-condition malformed-creation (type-error)
  ((place
     :initarg :place
     :reader place
     :documentation "Can be anything"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%Such abomination is now allowed to exist. Fix this ~%~a~%immidiately!~%" (place condition)))))




(defmacro type-safe (list body)
  `(handler-case ,body
     (error (e)
       (typecase e
         ,@(append
            (loop :for error-type :in list
                         :collect `(,error-type
                                    (format t "~a~%" e)
                                    (abort)))
            (list `(type-error
                    (format t "Something unregistered is messed up.~%")
                    (abort))
                  `(otherwise
                    (format t "Something is very wrong~%"))))))))



#||

(handler-case (error (make-condition 'type-error))
              (error (e)
                (typecase e
                  (race-error
                    (format t "1:~a~%" e)
                    (abort))
                  (skill-error
                    (format t "2:~a~%" e)
                    (abort))
                  (otherwise
                   (format t "3:NO IDEA~%")
                   (abort)))))



(define-condition race-error (type-error)
  ((wrong-race
     :type (and symbol (not race))
     :initarg :wrong-race
     :reader wrong-race
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The race ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-race condition))))))

(define-condition race-error (type-error)
  ((wrong-race
     :type (and symbol (not race))
     :initarg :wrong-race
     :reader wrong-race
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The race ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-race condition))))))

(define-condition race-error (type-error)
  ((wrong-race
     :type (and symbol (not race))
     :initarg :wrong-race
     :reader wrong-race
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The race ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-race condition))))))

(define-condition race-error (type-error)
  ((wrong-race
     :type (and symbol (not race))
     :initarg :wrong-race
     :reader wrong-race
     :documentation "Must be a typo"))
  (:report
    (lambda (condition stream)
      ;(declare (ignore condition))
      (format stream "~%The race ~a doesn't exist in our world. Please, chose one of the existing ones.~%" (%sym-to-str (wrong-race condition))))))
||#
