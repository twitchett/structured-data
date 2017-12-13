(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2) (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (count(:authors book)) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (if (:death-year author) false true))

(defn element-lengths [collection]
  (map count collection))

;; Use map to write the function (second-elements collection) that takes a
;; vector of vectors and returns a sequence of the second elements.
(defn second-elements [collection]
  (let [get-second (fn [c] (get c 1))]
    (map get-second collection)))

;; Write the function (titles books) that takes a collection of books and returns
;; their titles.
(defn titles [books]
  (let [get-title #(:title %)]
    (map get-title books)))

;; Write the function (monotonic? a-seq) that returns true if a-seq is monotonic
;; and otherwise false.
;; A sequence is monotonic if is either inceasing or decreasing.
(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

;; Write the function (stars n) that returns a string with n aterisks \*.
(defn stars [n]
  (apply str (repeat n "*")))

;; Write the function (toggle a-set elem) that removes elem from a-set if a-set
;; contains elem, and adds it to the set otherwise.
(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

;; Write the function (contains-duplicates? sequence) that takes a sequence as a
;; parameter and returns true if sequence contains some element multiple times.
;; Otherwise it returns false.
(defn contains-duplicates? [a-seq]
  (> (count a-seq) (count (set a-seq))))

;; Write the function (old-book->new-book book) that takes a book with the
;; previous representation (authors in a vector) and returns the same book in
;; the new representation (authors in a set).
(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

;; Write the function (has-author? book author) that returns true if author is
;; in the authors of book and otherwise false.
(defn has-author? [book author]
  (contains? (:authors book) author))

;; Write the function (authors books) that returns the authors of every book in
;; books as a set.
(require '[clojure.set :as set])
(defn authors [books]
  (apply set/union (map :authors books)))

;; Write the function (all-author-names books) that works like the previous one
;; and uses authors
(defn all-author-names [books]
  (set (map :name (authors books))))

;; Write the function (author->string author) that returns a string
;; representation of author as follows: [omitted]
(defn author->string [author]
  (let [{name :name
         birth-year :birth-year
         death-year :death-year} author]
    (str name
      (if birth-year
        (str " (" birth-year " - " death-year ")")))))

;; Write the function (authors->string authors) which takes a sequence of
;; authors as a parameter and returns a string representation of authors in the
;; following manner: [omitted]
(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

;; Write the function (book->string book) takes a single book as a parameter and 
;; returns a string representation of book as follows: [omitted]
(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

;; Write the function (books->string books) that takes a sequence of books as a
; parameter and returns a string representation of books like this: [omitted]
(defn books->string [books]
  (if (empty? books)
    (str "No books.")
    (str (count books) " book" (if (> (count books) 1) "s") ". "
        (apply str (interpose ". " (map book->string books))) ".")))

;; Write the function (books-by-author author books).
(defn books-by-author [author books]
  (filter #(has-author? % author) books))

;; Write the function (author-by-name name authors) that takes a string name and
;; a sequence of authors and returns an author with the given name if one is
;; found. If one is not found, then nil should be returned.
(defn author-by-name [name authors]
  (some #(if (= name (:name %)) % nil) authors))

;; Write the function (living-authors authors) that takes a sequence of authors
;; and returns those that are alive. 
(defn living-authors [authors]
  (filter #(alive? %) authors))

;; Write the function (has-a-living-author? book) that returns true if book has
;; a living author, and otherwise false.
(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

;; Write the function (books-by-living-authors books) that takes a sequence of
;; books as a parameter and returns those that have a living author.
(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %) books))
