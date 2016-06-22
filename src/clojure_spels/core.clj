;; A Clojure game based on http://www.lisperati.com/clojure-spels/casting.html

(ns clojure-spels.core)

;; Objects in the game
(def objects '(whiskey-bottle bucket frog chain))

;; The whole game map.
;; Only 3 rooms
;; For each room there is:
;;   A description of the room
;;   Subsequent lists of:
;;     Direction, path-to-location, New location 
(def game-map
  {'living-room '((you are in the living room of a wizard's house.  There is a wizard snoring loudly on the couch)
                  (west door garden)
                  (upstairs stairway attic))

   'garden '((you are in a beautiful garden.  There is a well in front of you)
             (east door living-room))

   'attic '((you are in the attic of the wizards house.  There is a giant welding torch in the corner)
            (downstairs stairway living-room))
   })


;; The objects in each location
(def object-locations {'whiskey-bottle 'living-room
                       'bucket 'living-room
                       'chain 'garden
                       'frog 'garden})


;; The starting location in the game
(def location 'living-room)


;; Function to describe your location
(defn describe-location [location game-map]
  (first (location game-map)))


;; Function to describe a path
(defn describe-path [path]
  `(there is a ~(second path) going ~(first path) from here - ))


;; Function to describe all paths in the location
(defn describe-paths [location game-map]
  (apply concat
         (map describe-path (rest (location game-map)))
  )
)

;; Full namespaces are shown when using backticks
;; Function to eliminate the full namespaces
(defn spel-print [lst] (map #(symbol (name %)) lst))


;; Function that determines if an object is at the location
(defn is-at? [obj loc obj-locs] (= (obj obj-locs) loc))


;; Function to describe the objects in the location
;; Objects are on the floor
(defn describe-floor [loc objs obj-locs]
  (apply concat 
         (map (fn [x] `(you see a ~x on the floor - ))
              (filter (fn [x] (is-at? x loc obj-locs)) objs)) ))

;; Function to print out the location, paths and objects on the floor
(defn look []
  (spel-print (concat (describe-location location game-map)
                      (describe-paths location game-map)
                      (describe-floor location objects object-locations) )))


;; Function that changes the location depending on the direction you want to go
(defn walk-direction [direction]
  (let [next (first (filter (fn [x] (= direction (first x))) (rest (location game-map))))]
    (cond next (do (def location (nth next 2))  (look))
                   :else '(you cannot go that way -) )))


;; Macro that replaces "walk" over the function walk-direction
(defmacro walk [direction] `(walk-direction '~direction))


;; Function to pick up objects
;; Adds to new object-locations with the object and "body"
(defn pickup-object [object]
  (cond (is-at? object location object-locations)
        (do
          (def object-locations (assoc object-locations object 'body))
          `(you are now carrying the ~object))
        :else '(you cannot get that.)))


;; Macro that replaces pickup-object with pickup
(defmacro pickup [object] `(spel-print (pickup-object '~object)))


;; Function that shows what you have on your body (inventory)
(defn inventory [] (filter (fn [x] (is-at? x 'body object-locations)) objects))


;; Function that tells you have if you have an object
(defn have? [obj] #(some #{obj} (inventory)))

;; Global initial variable if chain is welded
(def chain-welded false)

(defn weld [subject object]
  (cond (and (= location 'attic)
             (= subject 'chain)
             (= object 'bucket)
             (have? 'chain)
             (have? 'bucket)
             (not chain-welded))
        (do (def chain-welded true)
            '(the chain is now securely welded to the bucket -))
        :else '(you cannot weld that -)))

;; Global initial variable if bucket is filled with something (water?)
(def bucket-filled false)

(defn dunk [subject object]
  (cond (and (= location 'garden)
             (= subject 'bucket)
             (= object 'well)
             (have? 'bucket)
             chain-welded)
        (do (def bucket-filled true)
            '(the bucket is now filled with water))
        :else '(you cannot dunk that)))


;; Macro that can create defns like weld and dunk
(defmacro game-action [cmd subj obj place & args]
  `(defmacro ~cmd [subject# object#]
     `(spel-print (cond (and (= location '~'~place)
                             (= '~subject# '~'~subj)
                             (= '~object# '~'~obj)
                             (have? '~'~subj))
                        ~@'~args
                        :else '(I cannot ~'~cmd that)))))


;; weld using game-action macro
(game-action weld chain bucket attic
             (cond (and (have? 'bucket) (def chain-welded true))
                   '(the chain is now securely welded to the bucket -)
             :else '(you do not have a bucket -)))

;; dunk using game-action macro
(game-action dunk bucket well garden
             (cond chain-welded
                   (do (def bucket-filled true)
                       '(the bucket is now filled with water))
                   :else '(you cannot dunk that)))


;; Function to splash water on the wizard
(game-action splash bucket wizard living-room
             (cond (not bucket-filled) '(the bucket has nothing in it -)
                   (have? 'frog) '(the wizard awakens and see that you stole his frog - 
                                       He is upset and banishes you to the netherworlds -
                                       You lose!!!  THE END))
             :else '(the wizard awakens and greets you warmly -
                                He hands you the magic low carb donut -
                                You win!!!  THE END))


;; Start Game
(look)
