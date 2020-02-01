



#||
(make-action run Alex
  (%check-success
   (%roll :d 6 :times (+ (get-str creature)
                         (get-skill creature :skill-name 'running))) 5))

(make-action run Alex :mod x
  (d6 (str running) 5))

(make-action run Alex)
||#





(defcharacter 'Alex 'human
  (stats (4 3 7 2 2 2 2 2 2))
  (size 0)
  (traits ())
  (drawbacks ())
  (feats ())
  (items ()))
