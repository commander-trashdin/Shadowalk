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
;; This is GUI interface in McClim, prototype.
;;
;; Code:

(in-package :rpg)

;;; Define a application-frame (a.k.a. application window in traditional GUI's).

(defun make-setter (stat-field)
  (if (eql stat-field 'name)
      (lambda (gadget &aux (value (gadget-value gadget)))
        (funcall (fdefinition (list 'setf stat-field)) value (%selected-creature *application-frame*))
        (format *debug-io* "The ~a of ~a is now ~s~%"
                (%sym-to-str stat-field) (name (%selected-creature *application-frame*)) value)
        (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'creature-list) :force-p t))
      (lambda (gadget &aux (value (read-from-string (gadget-value gadget))))
        (funcall (fdefinition (list 'setf stat-field)) value (%selected-creature *application-frame*))
        (format *debug-io* "The ~a of ~a is now ~s~%"
                (%sym-to-str stat-field) (name (%selected-creature *application-frame*)) value)
        (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'creature-list) :force-p t))))

(defun creature-stat (stat-name)
  (make-pane 'label-pane
             :label stat-name
             :min-width 100
             :max-width 120))


(defun creature-stat-info (stat-field)
  (make-pane 'text-field
             :text-style (make-text-style :sans-serif :roman :normal)
             :value ""
             :min-width 40
             :max-width 60
             :activate-callback (make-setter stat-field)))


(defmacro make-stat-table (&body contents)
  `(tabling (:background +grey+
             :max-width 500)
     ,@(loop :for row :in contents
             :collect `(list ,@(loop :for elem :in row
                                     :collect `(creature-stat ,elem)
                                     :collect `,(read-from-string elem))))))

#||
(defun make-test-label2 (ax ay)
  (labelling (:label (format nil "~(~S~)" (list ax ay))
                     :align-x ax
                     :label-alignment ay
                     :foreground +WHITE+
                     :background +PALETURQUOISE4+
                     :text-style (make-text-style :sans-serif :roman :normal))))
||#

(define-application-frame error-message ()
  ((message
    :type string
    :initarg :message
    :reader message))
  (:layouts
   (default
    (vertically (:max-width 100 :min-width 100
                 :max-height 50 :min-height 50)
      (make-pane 'label-pane :label (message *application-frame*))
      (make-pane 'push-button
                 :label "OK"
                 :max-width 50
                 :min-width 50
                 :max-height 30
                 :min-height 30
                 :activate-callback
                 (lambda (&rest _)
                   (declare (ignore _))
                   (frame-exit *application-frame*)))))))

(define-application-frame Shadowalk ()
  ((%selected-creature
    :type (or creature null)
    :initform nil
    :accessor %selected-creature))
  (:pointer-documentation t)
  (:panes
   (int :interactor :height 400 :width 600)
   (name (creature-stat-info 'name))
   (race (creature-stat-info 'race))
   (age (creature-stat-info 'age))
   (gender (creature-stat-info 'gender))
   (strength (creature-stat-info 'strength))
   (agility (creature-stat-info 'agility))
   (constitution (creature-stat-info 'constituion))
   (charisma (creature-stat-info 'charisma))
   (perception (creature-stat-info 'perception))
   (intuition (creature-stat-info 'intuition))
   (will (creature-stat-info 'will))
   (logic (creature-stat-info 'logic))
   (reaction (creature-stat-info 'reaction))
   ;(strength (creature-stat-info 'strength))
   (creature-list :application :incremental-redisplay t :display-function 'display-names))
  (:layouts
   (default
    (horizontally (:equalize-height t)
                                        ;:max-width 7000 :max-height 3000
      (vertically ()
        (labelling (:label "Creature")
          (vertically (:equalize-width t)
            (horizontally (:equalize-height t)
              (labelling (:label "Character data")
                (tabling (:background +grey+
                          :max-width 500)
                  (list (creature-stat "Name") name
                        (creature-stat "Race") race)
                  (list (creature-stat "Age") age
                        (creature-stat "Gender") gender))))
            (labelling (:label "Stats"
                        :max-width 500)
              (make-stat-table
                ("Strength" "Agility" "Constitution")
                ("Charisma" "Perception" "Intuition")
                ("Will" "Logic" "Reaction")))))

        int)
      creature-list))))


(define-shadowalk-command (com-quit-address-book :menu "Quit")
   ()
 (frame-exit *application-frame*))

(define-presentation-type creature ())

(define-presentation-method present ((object creature) (type creature) stream view &key)
  (declare (ignore view))
  (write-string (format nil "~a, ~a" (name object) (%sym-to-str (race object))) stream))

(define-command (com-test :command-table Shadowalk)
    ((creature creature :gesture :select))
  (setf (%selected-creature *application-frame*) creature)
  (loop :for field :in '(name race strength agility constitution charisma perception intuition will logic reaction)
        :do (setf (gadget-value (find-pane-named *application-frame* field))
                  (format nil "~a" (let ((val (funcall (fdefinition field) (%selected-creature *application-frame*))))
                                     (if (symbolp val)
                                         (%sym-to-str val)
                                         val))))))



(defmethod display-names ((frame Shadowalk) stream)
  (maphash (lambda (k v)
             (declare (ignore k))
             (updating-output (stream :unique-id v)
               (present v 'creature :stream stream)
               (terpri stream)))
           *creatures*))


(defun app-main ()
  (declare (optimize (safety 3) (debug 3)))
  (handler-bind ((malformed-creation
                   (lambda (self)
                     (run-frame-top-level
                      (make-application-frame
                       'error-message
                       :calling-frame *application-frame*
                       :message
                       (format nil "Such abomination is now allowed to exist. Fix this ~a immidiately!" (place self))))
                     (abort))))
    (run-frame-top-level (make-application-frame 'Shadowalk))))


#||
(horizontally (
                      (make-pane 'label-pane
                                 :label "Strength"
                                 :min-width 60
                                 :max-width 200)
                      (clim-extensions:lowering ()
                        (make-pane 'text-field
                                   :name 'strength
                                   :value "0"
                                   :min-width 10
                                   :max-width 500))
                      (make-pane 'label-pane
                                 :label "Strength")
                      (make-pane 'text-field
                                 :name 'benchmark-times
                                 :value "100")
                      (make-pane 'label-pane
                                 :label "Strength")
                      (make-pane 'text-field
                                 :name 'benchmark-times
                                 :value "100")))
||#

