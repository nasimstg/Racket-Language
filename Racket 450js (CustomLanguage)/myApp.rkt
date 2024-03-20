#lang s-exp "cs450js-lang.rkt"

(bind/rec [fac
  (fn (n)
    (iffy (== n 0)
          1
          (* n (fac (- n 1)))))]
          (chk=? (fac 0) 1)
          (chk=? (fac 1) 1)
          (chk=? (fac 5) 120))

(bind/rec [filt
           (fn (pred lst)
      (iffy (emp? lst)
            (mk-lst)
            (iffy (pred (fst lst))
                  (cns (fst lst) (filt pred (rst lst)))
                  (filt pred (rst lst)))))]
          (chk=? (filt (fn (x) (> x 2)) (mk-lst 1 2 3 4 5)) (mk-lst 3 4 5)))

(bind/rec [qsort
  (fn (lst)
    (iffy (emp? lst)
      (mk-lst)
      (bind [pivot (fst lst)]
        (bind [rest (rst lst)]
          (bind/rec [filter
                      (fn (pred lst)
                        (iffy (emp? lst)
                          (mk-lst)
                          (bind [head (fst lst)]
                            (bind [tail (rst lst)]
                              (iffy (pred head)
                                (cns head (filter pred tail))
                                (filter pred tail))))))]
            (bind [smaller (filter (fn (x) (< x pivot)) rest)]
              (bind [larger (filter (fn (x) (> x pivot)) rest)]
                (append (qsort smaller) (mk-lst pivot) (qsort larger)))))))))]

(chk=? (qsort (mk-lst 3 2 5 1 7 6)) (mk-lst 1 2 3 5 6 7)))

(bind/rec [gcd
  (fn (a b)
    (iffy (== b 0)
          a
          (gcd b (mod a b))))]
          (chk=? (gcd 12 8) 4))


;; 5. sierpinski : Nat -> 2htdp/Image
(bind/rec [sierpinski
           (fn (depth)
               (iffy (= depth 0)
      (triangle 8 "solid" "black")
      (bind [smaller (sierpinski (- depth 1))]
            (above (beside smaller smaller)
                   smaller))))]
  (sierpinski 5))
