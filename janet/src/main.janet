(use judge)

(defn item [name quality sell-in] @{:name name :quality quality :sell-in sell-in})
(test (item "foo" 8 9) @{:name "foo" :quality 8 :sell-in 9})

(defn add-quality [i a] (put i :quality (min 50 (max 0 (+ a (i :quality)))))) 
(test ((add-quality (item "foo" 25 0) +10) :quality) 35)
(test ((add-quality (item "foo" 25 0) -30) :quality) 0)
(test ((add-quality (item "foo" 25 0) -10) :quality) 15)
(test ((add-quality (item "foo" 25 0) +30) :quality) 50)

(defn quality-change-picker [{:name n :quality q :sell-in s}]
  (match n
    "Backstage passes to a TAFKAL80ETC concert" 
      (cond
        (>= 0 s)  (- 0 q)
        (>= 5 s)  +3
        (>= 10 s) +2
                  +1)
    "Aged Brie" +1
    _ -1))
(test (quality-change-picker (item "Aged Brie" 10 3)) 1)
(test (quality-change-picker (item "yo" 10 3)) -1)
(let [pass "Backstage passes to a TAFKAL80ETC concert"] 
  (test (quality-change-picker (item pass 10 11)) 1)
  (test (quality-change-picker (item pass 10 10)) 2)
  (test (quality-change-picker (item pass 10 9)) 2)
  (test (quality-change-picker (item pass 10 6)) 2)
  (test (quality-change-picker (item pass 10 5)) 3)
  (test (quality-change-picker (item pass 10 4)) 3)
  (test (quality-change-picker (item pass 10 1)) 3)
  (test (quality-change-picker (item pass 10 0)) -10)
  (test (quality-change-picker (item pass 10 -1)) -10))

(defn is-sulfuras? [item] (= (item :name) "Sulfuras, Hand of Ragnaros"))
(test (is-sulfuras? (item "foo" 0 0)) false)
(test (is-sulfuras? (item "Sulfuras, Hand of Ragnaros" 0 0)) true)

(defmacro double [x] ~(set ,x (* 2 ,x)))
(test (do (var foo 2) (double foo) foo) 4)

(defn is-conjured? [item] (string/has-prefix? "Conjured" (item :name)))
(test (is-conjured? (item "foo" 0 0)) false)
(test (is-conjured? (item "Conjured foo" 0 0)) true)

(defn update-quality [items]
  (loop [item :in items :when (not (is-sulfuras? item))]
    (var change (quality-change-picker item)) 
    (when (> 1 (item :sell-in)) (double change))
    #(when (is-conjured? item) (double change))
    (add-quality item change)
    (put item :sell-in (- (item :sell-in) 1)))
  items)
(test (update-quality [(item "foo" 8 9)]) [@{:name "foo" :quality 7 :sell-in 8}])

(defn run [days items]
  (loop [:before (print "OMGHAI!")
         i :range-to [0 days]
         :before (print (string "-------- day " i " --------"))
         :before (print "name, sellIn, quality")
         :after (print "")
         :after (update-quality items)
         {:name n :quality q :sell-in s} :in items]
    (print (string n ", " s ", " q))))
