(in-package #:robby)

;; generics

(defgeneric execute-action (game-state next-action-index)
  (:documentation "Execute an action on the board, given the action index.
Actions are described by the *VALID-MOVES* parameter. Returns the points
given by the current action, and updates the game state with Robby's new
position, the current total score and the board state, as side effects.

Each move is rewarded or penalized as follows:

- Hitting a wall: -5 points
- Moving towards a can: 1 point
- Picking up a can: 10 points
- Attempt to pickup can where there is none: -1 point
- Otherwise: 0 points

Points never go below 0."))

(defgeneric execute-next-action (game-state)
  (:documentation "Execute a single action for the strategy enclosed on the
game state's specimen, for the current board state and Robby's position,
then return the amount of points given by that action. The action will also
update the game state, modifying the board if necessary, along with Robby's
position and the total score of the gameplay."))

(defgeneric measure-session (specimen)
  (:documentation "Measure a session for a specimen.
Each session generates a new board with a random number of cans, and makes
Robby perform a number of consecutive actions in it, defined by
*ACTIONS-PER-SESSION*."))

(defgeneric calculate-fitness (specimen)
  (:documentation "Calculates the fitness of a specimen in the current thread.
The fitness of a specimen is the average fitness calculated through all training
sessions. The amount of training sessions is defined by *NUM-TRAINING-SESSIONS*.

This method should be used in junction with worker threads when generating a
future that calculates the fitness for this given specimen."))

(defgeneric fitness (specimen)
  (:documentation "Get fitness future from a specimen. If no fitness is assigned,
a future will be spawned to calculate it through one of the workers."))


;; impl

(defmethod fitness ((object specimen))
  (unless (specimen-fitness object)
    (setf (specimen-fitness object)
          (lparallel:future (calculate-fitness object))))
  (specimen-fitness object))


(defmethod calculate-fitness ((object specimen))
  (float
   (/ (loop repeat *num-training-sessions*
            sum (measure-session object))
      *num-training-sessions*)))

(defun vision->index (north west here east south)
  "Converts Robby's current view of the board into an index for lookup on the
actual strategy vector (specimen).

Each position perceived by Robby is a vision of a board place that can contain
empty (0), a can (1) or a wall (2), therefore comprising a trit of a trinary
number. The parameters describe each trit from right to left. The lookup index
is then calculated from these positions as a base-10 number."
  (labels ((description->trit (place-element)
             (case place-element
               (empty 0)
               (can 1)
               (wall 2)
               (otherwise
                (error "I don't know what ~a is" place-element))))
           (trit->decimal (trit position)
             (* trit (expt 3 position)))
           (enumerate (list)
             (loop for elt in list
                   for i from 0
                   collect (list elt i))))
    (reduce
     #'+
     (mapcar (lambda (pair)
               (apply #'trit->decimal pair))
             (enumerate
              (mapcar #'description->trit
                      (list north west here east south)))))))

(defun get-environment-perception (position board)
  "Returns a list of Robby's perceived environment according to its position.
The position is expected to be a vector.
This list is comprised respectively of states describing north, west, current
location, east and south. These locations are symbols that can be one of EMPTY,
CAN or WALL."
  (labels ((position->description (x y)
             (cond ((not (position-valid-p x y)) 'wall)
                   ((aref board x y) 'can)
                   (t 'empty))))
    (let ((position (coerce position 'list)))
      (mapcar (lambda (coord-pair)
                (apply #'position->description coord-pair))
              (list
               ;; north
               (list (first position) (1- (second position)))
               ;; west
               (list (1- (first position)) (second position))
               ;; here
               position
               ;; east
               (list (1+ (first position)) (second position))
               ;; south
               (list (first position) (1+ (second position))))))))

(defun get-next-action-index (position board)
  "Given Robby's position and the current state of the game board, return the
index of the next action to be performed by Robby, according to its perception
of the environment."
  (apply #'vision->index
         (get-environment-perception position board)))

(defmethod execute-action ((state game-state) index)
  (when (= index 6)
    ;; Random action
    (setf index (random 6 *rnd*)))
  (let* ((position (coerce (robby-position state) 'list))
         (new-position
           (case index
             (0 (list (first position) (1- (second position)))) ; go north
             (1 (list (first position) (1+ (second position)))) ; go south
             (2 (list (1+ (first position)) (second position))) ; go east
             (3 (list (1- (first position)) (second position))) ; go west
             ((or 4 5) position) ; Action is in current place
             (otherwise (error "Unknown action #~a~%" index))))
         (points 0))
    (labels ((has-can-p (position)
               (aref (game-board state)
                     (first position)
                     (second position)))
             (pickup-can (position)
               (setf (aref (game-board state)
                           (first position)
                           (second position)) nil)))
      (case index
        ((or 0 1 2 3)
         (if (not (position-valid-p (first new-position)
                                    (second new-position)))
             ;; When hitting a wall, don't move and take a 5-point penalty
             (decf points 5)
             (progn
               ;; If moved towards can, get a 1-point reward
               (when (has-can-p new-position)
                 (incf points))
               ;; Update position
               (setf (robby-position state)
                     (coerce new-position 'vector)))))
        (4 (if (has-can-p position)
               ;; If there is a can on the current location, pick it up
               ;; and reward 10 points
               (progn (pickup-can position)
                      (incf points 10))
               ;; Otherwise do nothing and fine a 1-point penalty
               (decf points)))
        (5 (progn)) ; Stay where you are, do nothing, get no points
        (otherwise (error "Unknown action #~a~%" index))))
    ;; Update score
    (setf (game-score state)
          (let ((new-score (+ (game-score state) points)))
            (if *allow-negative-score*
                new-score
                (max new-score 0))))
    points))

(defmethod execute-next-action ((state game-state))
  (let* ((action-index
           (get-next-action-index (robby-position state)
                                  (game-board state)))
         (next-action (aref (specimen-vector (game-strategy state))
                            action-index)))
    (execute-action state next-action)))

(defmethod measure-session ((object specimen))
  (let ((state (make-game-state object)))
    (loop for i from 0 below *actions-per-session*
          do (execute-next-action state)
          finally (return (game-score state)))))
