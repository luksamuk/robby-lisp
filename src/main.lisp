(in-package #:robby)

(defparameter *population* nil
  "Population to be evolved.")

(defun init-session-workers ()
  "Initialize workers for calculating specimen fitness. The number of workers
is always equal to twice the number of available CPU cores."
  (unless lparallel:*kernel*
    (setf lparallel:*kernel*
          (lparallel:make-kernel
           (* 2 (cpus:get-number-of-processors))))))

(defun start ()
  "Start the application and evolve a population of specimen, according
tho the application's parameters."
  (init-session-workers)
  (let ((population (generate-initial-population)))
    (evolve population
            :debrief-fn
            (lambda (generation population)
              (format t "Generation #~a, Fittest: ~a~%"
                      generation (aref population 0))))))
