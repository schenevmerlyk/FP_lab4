
(defun insert (x sorted-list key test)
  (let ((x-key (funcall key x)))
    (if (or (null sorted-list) 
            (funcall test x-key (funcall key (car sorted-list))))
        (cons x sorted-list)
        (cons (car sorted-list) (insert x (cdr sorted-list) key test)))))

(defun insertion-sort-functional (list &key (key #'identity) (test #'<))
  (reduce (lambda (sorted x) (insert x sorted key test))
          list
          :initial-value nil))


(defun add-prev-reducer (&key (transform #'identity))
  (lambda (acc x)
    (let* ((prev (if (null acc) nil (caar (last acc))))
           (current (funcall transform x))
           (previous (if prev (funcall transform prev) nil)))
      (append acc (list (cons current previous))))))
        
        
(defun run-tests ()
  (format t "Testing: insertion-sort-functional~%")

  (format t "test 1: (3 1 4 1 5 9) ->~%")
  (format t "~A~%" (insertion-sort-functional '(3 1 4 1 5 9)))
  (format t "test 2: ((3 . 'c') (1 . 'a') (4 . 'd') (1 . 'b')) ->~%")
  (format t "~A~%" (insertion-sort-functional '((3 . "c") (1 . "a") (4 . "d") (1 . "b"))                                             :key #'car :test #'<))
  (format t "test 3, second parameter: '((3 . 'c') (1 . 'z') (4 . 'd') (1 . 'b')->~%")
  (format t "~A~%" (insertion-sort-functional '((3 . "c") (1 . "z") (4 . "d") (1 . "b"))
                                              :key #'cdr :test #'string<))
  (format t "Testing: insertion-sort-functional~%")                                     
  (let ((result (reduce (add-prev-reducer) '(1 2 3) :initial-value nil :from-end nil))
        (expected '((1 . NIL) (2 . 1) (3 . 2))))
    (if (equal result expected)
        (format t "Test 1 passed.~%")
        (format t "Test 1 failed: ~A~%" result)))

  ;; Тест 2: випадок із transform (наприклад, #'1+)
  (let ((result (reduce (add-prev-reducer :transform #'1+) '(1 2 3) :initial-value nil :from-end nil))
        (expected '((2 . NIL) (3 . 2) (4 . 3))))
    (if (equal result expected)
        (format t "Test 2 passed.~%")
        (format t "Test 2 failed: ~A~%" result)))

  ;; Тест 3: порожній список
  (let ((result (reduce (add-prev-reducer) '() :initial-value nil :from-end nil))
        (expected nil))
    (if (equal result expected)
        (format t "Test 3 passed.~%")
        (format t "Test 3 failed: ~A~%" result)))

  ;; Тест 4: список з одним елементом
  (let ((result (reduce (add-prev-reducer) '(42) :initial-value nil :from-end nil))
        (expected '((42 . NIL))))
    (if (equal result expected)
        (format t "Test 4 passed.~%")
        (format t "Test 4 failed: ~A~%" result)))

  ;; Тест 5: список із символами
  (let ((result (reduce (add-prev-reducer) '(a b c) :initial-value nil :from-end nil))
        (expected '((A . NIL) (B . A) (C . B))))
    (if (equal result expected)
        (format t "Test 5 passed.~%")
        (format t "Test 5 failed: ~A~%" result)))

  ;; Тест 6: користувацька transform-функція (зведення у квадрат)
  (let ((result (reduce (add-prev-reducer :transform (lambda (x) (* x x))) '(1 2 3) :initial-value nil :from-end nil))
        (expected '((1 . NIL) (4 . 1) (9 . 4))))
    (if (equal result expected)
        (format t "Test 6 passed.~%")
        (format t "Test 6 failed: ~A~%" result))))
  
  
(run-tests)

