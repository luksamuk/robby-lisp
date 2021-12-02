(in-package #:robby)

(defparameter *population-size* 200
  "Desired population size")

(defparameter *num-training-sessions* 100
  "Number of training sessions for each specimen.")

(defparameter *actions-per-session* 200
  "Max number of actions allowed per board session.")

(defparameter *can-placement-chance* 0.5
  "Probability of a having a can on each position of the board, when generating
a new random board.")

(defparameter *rnd* (make-random-state)
  "Global random state.")

(defparameter *board-side* 10
  "Number of positions on the side of a square board.")

(defparameter *valid-moves*
  '((0 north  #\N)
    (1 south  #\S)
    (2 east   #\E)
    (3 west   #\W)
    (4 pickup #\P)
    (5 halt   #\H)
    (6 random #\R))
  "Association list with the valid moves for Robby. Each element
is comprised of a move index, a move mnemonic and a unique character
representing that move.")

(defparameter *tournament-population-perc* 0.6
  "Percentage of the population to participate in each tournament selection
for the genetic algorithm.")

(defparameter *mutation-chance* 0.2
  "Percentage of mutation change per gene in a new specimen.")

(defparameter *num-generations* 1000
  "Number of generations to evolve a population.")

(defparameter *allow-negative-score* t
  "Whether or not a negative score for the specimen should be allowed.")


(defun random-by-chance (chance-perc)
  "Generate a random number with a specific change of success, given in
percentage."
  (let ((min-value (* 100 chance-perc)))
    (<= (1+ (random 100 *rnd*)) min-value)))
