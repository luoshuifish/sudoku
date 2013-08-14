;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;name:yuyongjiang
;;school:Buaa
;;Email:yuyongjianghit@163.com
(require racket/list)


;;每一格可能的值
(define VALUES (list 1 2 3 4 5 6 7 8 9))

;;定义空白处 B
(define B false)

;;一个测试用例
(define BD1          
  (list B 6 B 5 9 3 B B B
        9 B 1 B B B 5 B B
        B 3 B 4 B B B 9 B
        1 B 8 B 2 B B B 4
        4 B B 3 B 9 B B 1
        2 B B B 1 B 6 B 9
        B 8 B B B 6 B 2 B
        B B 4 B B B 8 B 7
        B B B 7 8 5 B 1 B))




(define ROWS
  (list (list  0  1  2  3  4  5  6  7  8)
        (list  9 10 11 12 13 14 15 16 17)
        (list 18 19 20 21 22 23 24 25 26)
        (list 27 28 29 30 31 32 33 34 35)
        (list 36 37 38 39 40 41 42 43 44)
        (list 45 46 47 48 49 50 51 52 53)
        (list 54 55 56 57 58 59 60 61 62)
        (list 63 64 65 66 67 68 69 70 71)
        (list 72 73 74 75 76 77 78 79 80)))


(define COLS
  (list (list 0  9 18 27 36 45 54 63 72)
        (list 1 10 19 28 37 46 55 64 73)
        (list 2 11 20 29 38 47 56 65 74)
        (list 3 12 21 30 39 48 57 66 75)
        (list 4 13 22 31 40 49 58 67 76)
        (list 5 14 23 32 41 50 59 68 77)
        (list 6 15 24 33 42 51 60 69 78)
        (list 7 16 25 34 43 52 61 70 79)
        (list 8 17 26 35 44 53 62 71 80)))

(define BOXS
  (list (list  0  1  2  9 10 11 18 19 20)
        (list  3  4  5 12 13 14 21 22 23)
        (list  6  7  8 15 16 17 24 25 26)
        (list 27 28 29 36 37 38 45 46 47)
        (list 30 31 32 39 40 41 48 49 50)
        (list 33 34 35 42 43 44 51 52 53)
        (list 54 55 56 63 64 65 72 73 74)
        (list 57 58 59 66 67 68 75 76 77)
        (list 60 61 62 69 70 71 78 79 80)))

;;
(define (read-square bd p)
  (list-ref bd p))


;;形成新的数独局面
(define (fill-square bd p nv)
  (append (take bd p)
          (list nv)
          (drop bd (add1 p))))


;;判断整个局面是否填充完毕
(define (full bd)
  (not (member false bd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;first-box输助函数
(define (first-box-temp bd location)
    (if (null? bd)
      false
      (if (boolean? (car bd))
          location
          (first-box-temp (cdr bd) (+ location 1)))))
          
;;计算整个局面的第一个格子的位置
(define (first-box bd)
  (first-box-temp bd 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;计算集合的并集
(define (union set1 set2)
  (cond
    ((null? set1) set2)
    ((member? (car set1) set2) (union (cdr set1) set2))
    (else (cons (car set1)
                (union (cdr set1) set2)))))
;;计算集合的交集
(define (intersect set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2)
     (cons (car set1)
           (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2))))

;;计算集合的补集
(define (complement universal set1)
  (cond
    ((null? universal) '())
    ((member? (car universal) set1) (complement (cdr universal) set1))
    (else 
     (cons (car universal) (complement (cdr universal) set1)))))
                  

;;Todo 4 由局面及索引求得其列号
(define (find-col bd index)
  (remainder index 9 ))

(define (read-col-temp bd l)
  (if (null? l)
      empty
      (cons (list-ref  bd (car l)) (read-col-temp bd (cdr l)))))

(define (read-col bd index)
  (read-col-temp bd (list-ref  COLS (find-col bd index))))

;;由局面及索引求得其行号
(define (find-row bd index)
  (floor  (/ index   9)))

(define (read-row-temp bd l)
  (if (null? l)
      empty
      (cons (list-ref  bd (car l)) (read-row-temp bd (cdr l)))))

(define (read-row bd index)
  (read-row-temp bd (list-ref  ROWS (find-row bd index))))

;;由局面及索引求得其Box号
(define (find-box-temp bd index count box)
  (if (member? index (car box))
      count
      (find-box-temp bd index (+ count 1) (cdr box))))
(define (find-box bd index)
  (find-box-temp bd index 0 BOXS))

(define (read-box-temp bd l)
  (if (null? l)
      empty
      (cons (list-ref  bd (car l)) (read-box-temp bd (cdr l)))))
(define (read-box bd index)
  (read-box-temp bd (list-ref  BOXS (find-box bd index))))
  
  
      

;;当前局面位置可能放的值
(define (maybe-numbers bd index)
  (intersect (complement VALUES (read-box bd index))
             (intersect (complement VALUES (read-col bd index))
             (complement VALUES (read-row bd index)))))
  
  


;;数独求解器 list -> list or false
(define (solve bd)
  (if (full bd)
          bd
      (local [(define result  (solve-temp (maybe-numbers bd (first-box bd)) bd (first-box bd)))]
        result)))

(define (solve-temp maybe-numbers bd p)
  (if (null? maybe-numbers)
      false
      (local [(define result (solve (fill-square bd p (car maybe-numbers))))]
        (if (boolean? result)
            (solve-temp (cdr maybe-numbers) bd p)
            result))))
            
      
          
        
        
        
        


