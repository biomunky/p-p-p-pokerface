(ns p-p-p-pokerface)

;; (defn suit [card]
;;   (let [[_ s] card] (str s)))

(defn suit [[_ b]] (str b))

;; (defn rank [card]
;;   (let [[rnk _] card
;;         replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
;;     (if (Character/isDigit rnk)
;;       (Integer/valueOf (str rnk))
;;       (replacements rnk))))

(defn rank [[r _]]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r) (Integer/valueOf (str r))
        (replacements r))))

(defn is-rank? [hand of-rank]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (== of-rank (apply max (vals freqs)))))

(defn pair? [hand]
  (is-rank? hand 2))

(defn three-of-a-kind? [hand]
  (is-rank? hand 3))

(defn four-of-a-kind? [hand]
  (is-rank? hand 4))

;; A flush is a poker hand such as Q♣ 10♣ 7♣ 6♣ 4♣.
;; All five cards are of the same suit, but not in sequence.
;; (defn flush? [hand]
;;   (let [suits (map suit hand)
;;         ranks (map rank hand)
;;         freqs (frequencies suits)
;;         contiguous-rank (range (apply min ranks) (+ 1 (apply max ranks)))]
;;     (and
;;      (not= contiguous-rank ranks)
;;      (== 5 (apply max (vals freqs))))))
;;(defn flush? [hand]
;;   (let [s (map suit hand)]
;;     (== 1 (count (set s)))))

(defn flush? [hand]
  (== 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= (sort (vals freqs)) [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        sorted-freqs (sort (vals freqs))]
    (or (= sorted-freqs [1 2 2])
        (= sorted-freqs [1 4]))))

;; A straight is a poker hand such as Q♣ J♠ 10♠ 9♥ 8♥.
;; Contains five cards of sequential rank in at least two different suits.
;; (defn straight? [hand]
;;   (let [ace-high (sort (map rank hand))
;;         ace-low (sort (replace {14 1} (map rank hand)))
;;         numb-suits (count (set (map suit hand)))]
;;     (and (>= numb-suits 2)
;;          (or (= ace-high (range (apply min ace-high) (+ 1 (apply max ace-high))))
;;              (= ace-low  (range (apply min ace-low)  (+ 1 (apply max ace-low))))))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low (sort (replace {14 1} ace-high))]
    (or (= ace-high (range (apply min ace-high) (+ 1 (apply max ace-high))))
        (= ace-low  (range (apply min ace-low)  (+ 1 (apply max ace-low)))))))


;; A straight flush is a hand that contains five cards in sequence, all of the same suit.
;; e.g. Q♣ J♣ 10♣ 9♣ 8♣.
;; A hand that meets the requirements of both a straight, and a flush)
;; ** except that the number of suits constraint appears to have been dropped
;; (defn straight-flush? [hand]
;;   (let [one-suit? (count (set (map suit hand)))
;;         is-flush? (flush? hand)
;;         ace-high (sort (map rank hand))
;;         ace-low (sort (replace {14 1} (map rank hand)))]
;;     (and one-suit? is-flush?
;;          (or (= ace-high (range (apply min ace-high) (+ 1 (apply max ace-high))))
;;              (= ace-low  (range (apply min ace-low)  (+ 1 (apply max ace-low))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0))
