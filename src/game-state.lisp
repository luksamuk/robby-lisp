(in-package #:robby)

(defclass game-state ()
  ((%board :initarg :board
           :accessor game-board
           :type array)
   (%strategy :initarg :strategy
              :accessor game-strategy
              :type specimen)
   (%position :accessor robby-position
              :initform (make-array 2
                                    :initial-contents '(0 0)
                                    :element-type 'fixnum)
              :type (vector fixnum))
   (%score :initform 0
           :accessor game-score
           :type fixnum))
  (:documentation "Describes the state of a play of the Robby game, for a given
strategy."))

(defmethod print-object ((object game-state) stream)
  (format stream "#<GAME-STATE :SCORE ~a :POSITION ~a~%"
          (game-score object)
          (robby-position object))
  (let ((*standard-output* stream))
    (print-board (game-board object) 2))
  (format stream ":STRATEGY ~a>~%" (game-strategy object)))

(defun make-game-state (specimen)
  "Generate a new game state, with a new random board, for a given specimen
that will be treated as Robby's strategy."
  (make-instance 'game-state
                 :board (make-board)
                 :strategy specimen))

