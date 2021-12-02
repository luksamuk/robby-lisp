(in-package #:robby)

(defun make-board ()
  "Create a square array containing a random number of cans.
Each position on the array has a chance of having a can or not, described
through *CAN-PLACEMENT-CHANCE*."
  (let ((board (make-array (list *board-side* *board-side*)
                           :initial-element nil
                           :element-type 'boolean)))
    (loop for y from 0 to (1- *board-side*)
          do (loop for x from 0 to (1- *board-side*)
                   when (random-by-chance *can-placement-chance*)
                     do (setf (aref board x y) t)))
    board))

(defun print-board (board &optional (indentation 0))
  "Print a board to the console."
  (labels ((indent ()
             (loop repeat indentation do (princ #\Space)))
           (horizontal-rule (char)
             (indent)
             (princ #\Space)
             (loop repeat *board-side*
                   do (loop repeat 3 do (princ char)))
             (princ #\Space))
           (print-cell (line column)
             (princ #\Space)
             (princ (if (aref board line column) #\c #\.))
             (princ #\Space))
           (print-line (line)
             (indent)
             (princ #\|)
             (loop for column to (1- *board-side*)
                   do (print-cell line column))
             (princ #\|)
             (terpri)))
    (horizontal-rule #\_)
    (terpri)
    (loop for line from 0 to (1- *board-side*)
          do (print-line line))
    (horizontal-rule #\¯)
    (terpri)))

(defun position-valid-p (x y)
  "Checks whether the given X and Y coordinates describe a valid coordinate,
according to the parameter *BOARD-SIDE*."
  (and (>= x 0)
       (>= y 0)
       (< x *board-side*)
       (< y *board-side*)))
