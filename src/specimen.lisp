(in-package #:robby)

;;; A specimen is essentially a vector, a sequence of exactly 243 actions
;;; describing what Robby should do for each situation on the board.
;;;
;;; These situations relate to Robby's current view of the board. Robby uses
;;; a vision based on Moore's neighborhood, where it looks at four cardinal
;;; directions (North, South, East, West) and to wherever it is.
;;;
;;; Every time Robby looks at its neighborhood, the state of the five observed
;;; cells can be:
;;; 1. A wall (except for the center one);
;;; 2. Empty;
;;; 3. Contains a can.
;;;
;;; For each single neighborhood vision containing the state of the five
;;; observed cells, Robby performs the action ascribed to it. The actions are
;;; enumerated in *VALID-MOVES*, and are basically actions for walking on all
;;; four directions, plus the actions for picking up a can on the current
;;; location, staying put, and selecting an action at random.

(defun gen-raw-specimen ()
  "Generate a raw random specimen.
A raw specimen is a vector of random strategies. The number of strategies
is always 243, which is the max value of a trinary number of five digits.
This is determined by the facts that Robby can only look at its Moore
neighborhood including where it is (north, south, east, west, here) and
that a place on the board can have three states (empty, wall, can)."
  (make-array 243
              :element-type 'fixnum
              :initial-contents
              (loop repeat 243
                    collect (first (nth (random 7 *rnd*)
                                        *valid-moves*)))))

(defclass specimen ()
  ((%vec :initarg :data
         :accessor specimen-vector
         :documentation "Raw specimen strategy vector.")
   (%fitness :initform nil
             :accessor specimen-fitness
             :documentation "Fitness for the current specimen.
The fitness is actually a future which, when fulfilled, yields the
fitness for this specimen as a fixnum."))
  (:documentation "Represents a specimen, a collection of actions that
can be performed by Robby in specific situations."))

(defmethod print-object ((object specimen) stream)
  (let* ((vec (specimen-vector object))
         (beginning (coerce (subseq vec 0 5) 'list))
         (ending (coerce (subseq vec 238 243) 'list)))
    (format stream "#<SPECIMEN ~{~a~}...~{~a~} :FITNESS ~a {~x}>"
            beginning
            ending
            (let ((fitness (specimen-fitness object)))
              (if fitness
                  (if (lparallel:fulfilledp fitness)
                      (lparallel:force fitness)
                      "CALCULATING")
                  "??"))
            (sxhash object))))

(defun make-specimen ()
  "Creates a new specimen from random data."
  (make-instance 'specimen :data (gen-raw-specimen)))

(defgeneric print-specimen (specimen)
  (:documentation "Prints a specimen as letters for each move."))

(defmethod print-specimen ((object specimen))
  (format nil "~{~a~}"
          (mapcar (lambda (idx)
                    (third (nth idx *valid-moves*)))
                  (coerce (specimen-vector object) 'list))))

