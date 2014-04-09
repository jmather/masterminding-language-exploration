(ns com.jmather.mastermind)

(def SOLUTION_LENGTH 4)
(def MAX_GUESSES 10)
(def GUESS_WRONG "_")
(def GUESS_RIGHT "X")
(def GUESS_PRESENT "*")
(def CHOICES '("A" "B" "C" "D" "E" "F"))

(defn gameIntro
  "Print game introduction"
  []
  (do
    (println "Let's play Mastermind! The rules are easy, I promise.")
    (print "I will pick" (str SOLUTION_LENGTH) "letters out of a possible" (str (count CHOICES)))
    (println ".")
    (println "You will then get" MAX_GUESSES "chances to guess which letters I picked.")
    (println "For each letter you guess, I will answer as follows:")
    (println "\t" GUESS_WRONG "means you got it compltely wrong.")
    (println "\t" GUESS_PRESENT "means you guessed a letter I used, but it's in the wrong position.")
    (println "\t" GUESS_RIGHT "means you guessed that letter right!")
    (println "")))

(defn generateSolutionWithArgs
  "Generates a solution from the passed arguments"
  [choices
   picked
   remaining]
  (if (= remaining 0)
    (clojure.string/join "" picked)
    (loop [pick (rand-nth choices)]
      (if (= -1 (.indexOf picked pick))
        (generateSolutionWithArgs choices (conj picked pick) (- remaining 1))
        (recur (rand-nth choices))))))

(defn generateSolution
  "Generates a solution"
  []
  (generateSolutionWithArgs CHOICES () SOLUTION_LENGTH))

(defn askGuess
  "Ask the user for their guess"
  []
  (println "Available pegs:" (clojure.string/join ", " CHOICES))
  (print "Enter your guess (pick" SOLUTION_LENGTH)
  (print "): ")
  (flush)
  (clojure.string/upper-case (clojure.string/trim (str (read-line)))))

(defn isValidGuess
  "Is the given guess valid"
  [guess]
  (let [regex (clojure.string/join "" (conj '("]") (clojure.string/join "" CHOICES) "[^"))]
    (if (re-find (re-matcher (re-pattern regex) guess))
      false
      (if (= (count guess) SOLUTION_LENGTH)
        true
        false))))

(defn getGuess
  "Get a qualified guess from the user"
  []
  (loop [guess (askGuess)]
    (if (isValidGuess guess)
      guess
      (do
        (println "We were unable to understand your input. Please enter only the letters of your guess and press enter.")
        (recur (askGuess))))))

(defn analyzeGuess
  "Returns a string of the guess analysis"
  [full_solution full_guess]
  (loop [solution full_solution
         guess full_guess
         remaining SOLUTION_LENGTH
         result ()]
    (if (= remaining 0)
      (clojure.string/join "" (reverse result))
      (let [solution_letter (subs solution 0 1)
            guess_letter (subs guess 0 1)
            next_solution (subs solution 1)
            next_guess (subs guess 1)
            next_iteration (- remaining 1)]
        (if (= solution_letter guess_letter)
          (recur next_solution next_guess next_iteration (conj result GUESS_RIGHT))
          (if (> (.indexOf full_solution guess_letter) -1)
            (recur next_solution next_guess (- remaining 1) (conj result GUESS_PRESENT))
            (recur next_solution next_guess next_iteration (conj result GUESS_WRONG))))))))


(gameIntro)

(def solution (generateSolution))

(loop
  [guesses MAX_GUESSES]
  (if (= guesses 0)
    (println "You've ran out of guesses! Better luck next time!")
    (let [guess (getGuess)]
      (if (= guess solution)
        (println "You've guessed correctly, you win!")
        (do
          (println "Result:" (analyzeGuess solution guess))
          (recur (- guesses 1)))))))