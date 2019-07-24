#lang pl

   (: plPrefixContained : String String String String String -> (U String #f))
   (define (plPrefixContained a b c d e)
     (cond
      [(and (>= (string-length a) (string-length "pl"))
            (equal? (string-ref a 0) #\p)
            (equal? (string-ref a 1) #\l))
       a]
      [(and (>= (string-length b) (string-length "pl"))
            (equal? (string-ref b 0) #\p)
            (equal? (string-ref b 1) #\l))
       b]
      [(and (>= (string-length c) (string-length "pl"))
            (equal? (string-ref c 0) #\p)
            (equal? (string-ref c 1) #\l))
       c]
      [(and (>= (string-length d) (string-length "pl"))
            (equal? (string-ref d 0) #\p)
            (equal? (string-ref d 1) #\l))
       d]
      [(and (>= (string-length e) (string-length "pl"))
            (equal? (string-ref e 0) #\p)
            (equal? (string-ref e 1) #\l))
       e]
      [else #f]))
   (test (plPrefixContained "plaa" "ttplt" "pttt" "appl" "pplpl") => "plaa")
   (test
    (plPrefixContained "pyyyt" "pltplt" "ptteee" "abcd" "pplpl")
    =>
    "pltplt")
   (test (plPrefixContained "papapa" "pbpbpb" "plptt" "ppl" "pplpl") => "plptt")
   (test
    (plPrefixContained "lplyyyt" "tplt" "lptt" "plosjs" "pplpl")
    =>
    "plosjs")
   (test
    (plPrefixContained "lplyyyt" "tplt" "lptt" "lplpl" "pljushdb")
    =>
    "pljushdb")
   (test (plPrefixContained "" "" "" "" "") => false)
   (test
    (plPrefixContained "plsasa" "pljfub" "plkdef" "plhebdbd" "plhwhdwhdh")
    =>
    "plsasa")
   (test (plPrefixContained "a" "b" "d" "d" "plpl") => "plpl")
   (test (plPrefixContained "" "" "k" "lplpl" "pplpl") => false)





   (: longestString : (Listof Any) -> (U String #f))
   (define (longestString list)
     (: helper : String (Listof Any) -> (U String #f))
     (define (helper acc list)
       (cond
        [(null? list) acc]
        [(and (string? (first list))
              (string? acc)
              (string? acc)
              (> (string-length acc) 0)
              (< (string-length acc) (string-length (first list))))
         (helper (first list) (rest list))]
        [else (helper acc (rest list))]))
     (cond
      [(null? list) #f]
      [(and (string? (first list)) (> (string-length (first list)) 0))
       (helper (first list) (rest list))]
      [else (longestString (rest list))]))
   (test (longestString '(34 uuu 90)) => false)
   (test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")
   (test (longestString '(uu 56 oooo "" "")) => false)
   (test (longestString '("" "" "" "" "")) => false)
   (test (longestString '(uu 56 oooo "r" "rRR" "TTrrrT")) => "TTrrrT")
   (test
    (longestString '(uu 56 "oooopppppp" "r" "rRR" "TTrrrT"))
    =>
    "oooopppppp")
   (test
    (longestString '("abcdefghij" 56 "oooopppppp" "r" "rRR" "TTrrrT"))
    =>
    "abcdefghij")
   (test
    (longestString
     '("abcdefghij" "oooopppppp" "r" "rRR" "TTrrrTpppppppppppppp"))
    =>
    "TTrrrTpppppppppppppp")
   (test
    (longestString '("12345" "12345" "123456" "1234567" "12345678"))
    =>
    "12345678")



   (: shoreststring : (Listof Any) -> (U String #f))
   (define (shoreststring list)
     (: helper : String (Listof Any) -> (U String #f))
     (define (helper acc list)
       (cond
        [(null? list) acc]
        [(and (string? (first list))
              (string? acc)
              (> (string-length acc) (string-length (first list))))
         (helper (first list) (rest list))]
        [else (helper acc (rest list))]))
     (cond
      [(null? list) #f]
      [(string? (first list)) (helper (first list) (rest list))]
      [else (shoreststring (rest list))]))
   (test (shoreststring '(34 uuu 90)) => false)
   (test (shoreststring '(uu 56 oooo "r" "rRR" "TTT")) => "r")
   (test (shoreststring '(uu 56 oooo "r" "rRR" "")) => "")
   (test (shoreststring '(uu 56 "aaaa" "rfd" "rRRqq" "TTaaaT")) => "rfd")
   (test (shoreststring '("uu" "5asadadada6" oooo)) => "uu")
   (test (shoreststring '("" "" "")) => "")




   (: short&long-lists : (Listof (Listof Any)) -> (Listof (Listof Any)))
   (define (short&long-lists list) (map short&long-list list))
   (: short&long-list : (Listof Any) -> (Listof Any))
   (define (short&long-list lst)
     (cond
      [(null? lst) null]
      [(string? (first lst))
       (cons (shoreststring lst) (list (longestString lst)))]
      [else (short&long-list (rest lst))]))
   (test
    (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3))))
    =>
    '(("OP" "Benny") ()))
   (test
    (short&long-lists '((any "Benny" 10 "OP" "s" 8) (any Benny OP (2 3))))
    =>
    '(("s" "Benny") ()))
   (test
    (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ()))
    =>
    '(("5gg" "2 5 5") ("f" "f") ()))
   (test
    (short&long-lists '((any aaa bcbc any r) (any Benny OP (2 3))))
    =>
    '(() ()))
   (test
    (short&long-lists '((any "Benny" 10 "OP" "" 8) (any Benny OP (2 3))))
    =>
    '(("" "Benny") ()))
   (test
    (short&long-lists
     '((any "Benny" 10 "OP" "" "ppppppppppppppppp") ("aaaa" "bbb" "ccccc")))
    =>
    '(("" "ppppppppppppppppp") ("bbb" "ccccc")))


   (define-type KeyStack
     [EmptyKS]
     [Push Symbol String KeyStack])


   (: search-stack : Symbol KeyStack -> (U String #f))
   (define (search-stack sym stack)
     (: equals1s2 : Symbol Symbol String KeyStack -> (U String #f))
     (define (equals1s2 sym1 sym2 str stack3)
       (if (equal? sym1 sym2) str (search-stack sym1 stack3)))
     (cases stack [(Push x y stack2) (equals1s2 sym x y stack2)] [else #f]))


   (: pop-stack : KeyStack -> (U KeyStack #f))
   (define (pop-stack stack4)
     (cases stack4 [(Push a b stack5) stack5] [else #f]))
   (test (EmptyKS) => (EmptyKS))
   (test
    (Push 'b "B" (Push 'a "A" (EmptyKS)))
    =>
    (Push 'b "B" (Push 'a "A" (EmptyKS))))
   (test
    (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))
    =>
    (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
   (test
    (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
    =>
    "AAA")
   (test
    (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
    =>
    #f)
   (test
    (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
    =>
    (Push 'b "B" (Push 'a "A" (EmptyKS))))
   (test (pop-stack (EmptyKS)) => #f)


