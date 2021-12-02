(in-package #:robby)

(defun generate-initial-population ()
  "Generates an initial population of random specimen with uncalculated
fitness, which shall be calculated on demand."
  (make-array *population-size*
              :element-type 'specimen
              :initial-contents
              (loop repeat *population-size*
                    collect (make-specimen))))

(defun tournament-selection (population &optional (partner nil))
  "Perform a tournament selection among a population. The tournament will be
performed K times until we get the best match. A greater fitness improves
the chance for selection. Population is expected to be a vector of SPECIMEN,
with size *POPULATION-SIZE*.

The tournament will always seek a specimen different than PARTNER, if
any is given."
  
  (loop with best = nil
        repeat (truncate (* *population-size* *tournament-population-perc*))
        do (let ((current
                   (loop with individual = nil
                         do (setf individual
                                  (aref population
                                        (random *population-size* *rnd*)))
                         unless (eq individual partner)
                           return individual)))
             (when (or (null best)
                       (> (lparallel:force (fitness current))
                          (lparallel:force (fitness best))))
               (setf best current)))
        finally (return best)))

(defun mutate-raw-specimen (raw-specimen)
  "Mutate each gene in a raw specimen with a chance given by
*MUTATION-CHANCE*.

Mutation chance should preferably be small, and is rolled for each gene
in the genome. When performing a mutation, the gene will be replaced by
a number x such that 0 <= x <= 6.

The raw specimen is expected to be a vector containing 243 numbers,
such that 0 <= x <= 6 as well, representing Robby's moves for each
possible situation on the board."
  (loop for i below 243
        when (random-by-chance *mutation-chance*)
          do (setf (aref raw-specimen i) (random 7 *rnd*)))
  raw-specimen)

(defun generate-new-individuals (population)
  "Generates two new individuals from two different parents, selected
through tournament selection. The population is expected to be a vector
of SPECIMEN. This is performed by selecting a random cutting point for
both parents, and then by producing two children mixing their genome;
with a small probability, the numbers in each child are then mutated."
  (let* ((parent1 (tournament-selection population))
         (parent2 (tournament-selection population parent1))
         (cutting-point (random 243 *rnd*)))
    (let ((parent1-half1 (subseq (specimen-vector parent1) 0 cutting-point))
          (parent1-half2 (subseq (specimen-vector parent1) cutting-point))
          (parent2-half1 (subseq (specimen-vector parent2) 0 cutting-point))
          (parent2-half2 (subseq (specimen-vector parent2) cutting-point)))
      (list (make-instance
             'specimen
             :data (mutate-raw-specimen
                    (concatenate 'vector
                                 parent1-half1 parent2-half2)))
            (make-instance
             'specimen
             :data (mutate-raw-specimen
                    (concatenate 'vector
                                 parent2-half1 parent1-half2)))))))

(defun generate-new-generation (population)
  "Generates a new generation from an old population, with roughly the
same size as before. The new generation is created by breeding two specimen
to spawn two new individuals at a time, until the number of elements reach
at least *POPULATION-SIZE*."
  (loop while (< (length generated) *population-size*)
        append (generate-new-individuals population)
          into generated
        finally (return (coerce generated 'vector))))

(defun sort-by-fitness (population)
  "Sort population by fitness. This function should be used only by debugging
purposes to avoid forcefully calculating the fitness of all individuals."
  (sort population
        (lambda (a b) (> (lparallel:force (fitness a))
                    (lparallel:force (fitness b))))))

(defun evolve (population &key (debrief-fn nil) (debrief-step 10))
  "Evolves a population for a number of generations *NUM-GENERATIONS*.
Optionally allows debriefing every DEBRIEF-STEP generations. This will
then call DEBRIEF-FN with the generation number and a population sorted by
their fitness, respectively."
  (loop for i below *num-generations*
        when (and debrief-fn
                  (zerop (mod i debrief-step)))
          do (funcall debrief-fn i (sort-by-fitness population))
        do (setf population (generate-new-generation population))
        finally (return population)))
