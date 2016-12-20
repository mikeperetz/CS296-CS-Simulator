(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map {

    :home {
      :desc "Welcome! This is your apartment. You are just now starting your semester as a Computer Science major at UIUC! You have 30 days in total. Some things to note:\n 
        * At any point in time, you may (sleep), (study), (relax), or change locations. Given your heavy class schedule, you only have the chance to do one of these each day. \n 
        * There are 3 distinct locations. Your apartment (home), the library (grainger), and the testing center (testcenter). You may navigate to these by typing home, grainger, or testcenter respectively. \n 
        * On each of the 10th, 20th, and 30th days, you will have an exam. If you are not at the testing center by that date, you will miss your exam. \n 
        * You have 3 attributes to maintain: Insanity, Exhaustion, and Knowledge. Too much studying and not enough sleep and relaxation will take a large toll on your insanity and exhaustion levels. On the other hand, if you do not study enough, you will not do well on exams, even if you answer the questions correctly. It's a rough life. \n 
        * It is your job to experiment with different command/room combinations. There is not much room for error. Poor planning will result in sure death. \n 
        * You may type (status) to view your current stats. \n
        * Prepare to die a lot until you find your limits. Have fun! \n"
      :title "in your apartment"
    }

     :testingcenter {
      :desc "Welcome to the Testing Center. This is where you will be administered your computerised exam every 10 days. Note that your success depends not just on whether you can answer the question, but also on your knowledge rating."
      :title "at the testing center"
    }

     :grainger {
      :desc "This is the Grainger Engineering Library. It takes nearly an entire day just to find a seat..."
      :title "at Grainger"
    }
})

(def adventurer
  {:location :home
  ; :inventory #{}
   :day 1
   :insanity 0
   :exhaustion 0
   :knowledge 0
   :seen #{}
  }
)

(defn newday [player command]
  (let [normal (update-in player [:day] + 1)
        notslept (update-in normal [:exhaustion] + 1)
        studied (update-in notslept [:insanity] + 1)]

    (if (= command "studying")
      studied
    )

    (if (= command "sleeping")
      normal
    )

    (if (= command "relaxing")
      notslept
    )

    (if (= command "testing")
      studied
    )  
    studied
  )
)

(declare testing)

(defn showhelp [player]
  (println "look, help, quit, reset, sleep, study, relax, grainger, home, testcenter, status, a, b, c, d")
  player
)

(defn quit [player]
  (System/exit 0)
  player
 )

(defn reset [player]
  (let [rested (assoc-in player [:exhaustion] -1)
        destressed (assoc-in rested [:insanity] 0)
        dumb (assoc-in destressed [:knowledge] 0)
        gohome (assoc-in dumb [:location] :home)
        dayone (assoc-in gohome [:day] 0)]

    (newday dayone "sleeping")
  )
)

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn checkdeath [player]

  (if (or (or (>= (get-in player [:insanity]) 13) (> (get-in player [:exhaustion]) 3)) (>= (player :day) 31))

    (do
      (if (>= (get-in player [:insanity]) 13)
        (do
          (println "You went too long without relaxing and suffered a fatal heart attack. RIP.")
        (println "The game has been reset. You can play again, or type quit to exit")
          
        )
  
      )
      (if (> (get-in player [:exhaustion]) 3)
        (do
          (println "You went too long without sleeping and accidentally walked into Green St. traffic. RIP.")
        (println "The game has been reset. You can play again, or type quit to exit")
            
        )      
      )

       (if (>= (player :day) 31)
        (do
          (println "You survived! And you only suffered moderate brain damage from sleep deprivation. Can you find a more efficient path through the semester?")
          (println "The game has been reset. You can play again, or type quit to exit")  
        )
      )
       (reset player)
    )

    (if (and (not= (player :day) 0) (= (rem (player :day) 10) 0))
      (testing player (player :day))
      player
    )
  )
)

(defn sleeping [player]
  (let [normal (newday player "sleeping")
        rested (assoc-in normal [:exhaustion] (max (- (get-in normal [:exhaustion]) 3) -1))
        wellrested (assoc-in rested [:insanity] (max (- (get-in rested [:insanity]) 1) -1))
        anxious (update-in rested [:insanity] + 2)]

    (if (not= (get-in normal [:location]) :home)
      (do   
        (println "You rested well, but sleeping away from home took a toll on your mental health.")
        (println "Exhaustion -3, Insanity +2.")
        (checkdeath anxious)
      )
      ;else if we are at home, return rested
      (do
        (println "You rested well.")
        (println "Exhaustion -4, Insanity -1.")
        (checkdeath wellrested)
      )
    )
  ) 
)


(defn studying [player]
  (let [normal (newday player "studying")
        studied (update-in normal [:knowledge] + 1)
        anxious (update-in studied [:insanity] + 0)
        wellstudied (update-in anxious [:knowledge] + 2)
        extraanxious (update-in wellstudied [:insanity] + 1)]

    (if (= (get-in normal [:location]) :home)
      (do
        (println "You weren't entirely focused, but your studying was less stressful as a result.")
        (println "Knowledge +1.")
        (checkdeath anxious)
      )
      ;if studied elsewhere
      (do
        (println "You were very focused, but your studying was quite stressful as a result.")
        (println "Knowledge +3, Insanity +1.")
        (checkdeath extraanxious)
      )
    )
  ) 
)

(defn relaxing [player]
  (let [normal (newday player "relaxing")
        relaxed (assoc-in normal [:insanity] (max (- (get-in normal [:insanity]) 3) 0))
        wellrelaxed (assoc-in relaxed [:insanity] (max (- (get-in normal [:insanity]) 5) 0))
        wellrelaxrested (assoc-in wellrelaxed [:exhaustion] (max (- (get-in wellrelaxed [:exhaustion]) 1) 0))]


    (if (= (get-in normal [:location]) :home)
      (do
        (println "You laid on the couch, played some videogames, and watched an entire movie on Netflix. What a great day.")
        (println "Insanity -4, Exhaustion -1.")
        (checkdeath wellrelaxrested)
      )
      ;if studied elsewhere
      (do
        (println "You sat and talked with friends for a while. It was a nice break from working.")
        (println "Insanity -3.")
        (checkdeath relaxed)
      )
    )
  ) 
)

(defn answerQ [number]

  (cond 
    (= number 1)
      (do
        (println "What is the running time for accessing an element in an array?")
        (println "a O(1)")
        (println "b O(n)")
        (println "c O(logn)")
        (println "d O(n^2)")
        (to-keywords (read-line))   
      )
    (= number 2)
      (do
        (println "What is the worst-case running time for inserting into an unbalanced binary search tree?")
        (println "a O(1)")
        (println "b O(n)")
        (println "c O(logn)")
        (println "d O(n^2)")
        (to-keywords (read-line))  
      )
    (= number 3)
      (do
        (println "What is the worst-case running time for inserting into a balanced binary search tree?")
        (println "a O(1)")
        (println "b O(n)")
        (println "c O(logn)")
        (println "d O(n^2)")
        (to-keywords (read-line))  
      )
  )
)

(defn test1 [player]
  (let [knowledge (player :knowledge)
        answer (answerQ 1)]

    (match answer
         [:a] (if (>= knowledge 7) 2 1 )     
         _ (if (>= knowledge 7) 1 0 )     
    )
  )
)

(defn test2 [player]
  (let [knowledge (player :knowledge)
        answer (answerQ 2)]

    (match answer
         [:b] (if (>= knowledge 18) 2 1 )     
         _ (if (>= knowledge 18) 1 0 )     
    )
  )
)

(defn test3 [player]
  (let [knowledge (player :knowledge)
        answer (answerQ 3)]

   (match answer
         [:c] (if (>= knowledge 26) 2 1 )     
         _ (if (>= knowledge 26) 1 0 )     
    )
  )
)

(defn takeTest [player day]
  (do 
    (if (not= (get-in player [:location]) :testingcenter)
      -1
      (cond 
          (= day 10) 
            (do 
              (println "Starting Midterm 1")
              (test1 player)
            )
          (= day 20) 
            (do 
              (println "Starting Midterm 2")
              (test2 player)
            )
          (= day 30) 
            (do 
              (println "Starting Final")
              (test3 player)
            )
        )
    )
  )
)

(defn reveal [player]
  (do 
    (println player)
    player
  )
)

(defn testing [player day]
  (let [score (takeTest player day)
        normal (newday player "testing")
        aced (update-in normal [:insanity] - 3)
        passed (update-in normal [:insanity] + 4)
        failed (update-in normal [:insanity] + 8)
        missed (update-in normal [:insanity] + 10)]

    (if (not= (get-in normal [:location]) :testingcenter)
      (do
        (println "You aren't yet in the testing center! You will certainly miss your exam.")
        (println "Insanity +10.")
        (checkdeath missed)
      )
      ;if at testingcenter
      (do
        (cond 
          (= score 0) 
            (do 
              (println "You completely bombed your exam! Study harder next time.")
              (println "Insanity +8.")
              (checkdeath failed)
            )
          (= score 1) 
            (do 
              (println "You did below average on your exam. Study harder next time.")
              (println "Insanity +4.")
              (checkdeath passed)
            )
          (= score 2) 
            (do 
              (println "You aced your exam! Keep up the good work!")
              (println "Insanity -3.")
              (checkdeath aced)
            )
        )
      )
    ) 
  )
)


(defn status [player]
  (let [location (player :location)]
    (println)
    (println "===============================================")
    (println)
    (println "*** Day:"(get-in player [:day])"***\n")
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))))



(defn go [dir player]
  (let [normal (newday player "traveling")
       moved (assoc-in normal [:location] dir)]
   
    (checkdeath moved)
  )
)



(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         [:testcenter] (go :testingcenter player)
         [:grainger] (go :grainger player)
         [:home] (go :home player)
         [:sleep] (sleeping player)
         [:study] (studying player)
         [:relax] (relaxing player)
         [:status] (reveal player)
         [:help] (showhelp player)
         [:reset] (reset player)
         [:quit] (quit player)
         _ (do (println "I don't understand you.")
               player)
  )
)
 

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println " What do you want to do? (type help for commands)")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))