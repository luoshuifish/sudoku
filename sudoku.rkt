;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;name:yuyongjiang
;;school:Buaa
;;Email:yuyongjianghit@163.com
(require racket/list)


;;每一格可能的值
(define VALUES (list 1 2 3 4))

;;定义空白处 B
(define B false)

;;一个测试用例
(define BD1
  (list B B 3 4
        4 3 2 1
        2 1 4 3
        3 4 1 2))
(define BD2
  (list 1 2 3 4
        4 3 2 1
        2 1 4 3
        3 4 1 2))

(define BD3
  (list 1 2 3 4
        4 B 2 1
        2 1 B 3
        3 4 1 2))


(define ROWS
  (list (list 0 1 2 3)
        (list 4 5 6 7)
        (list 8 9 10 11)
        (list 12 13 14 15)))
(define COLS
  (list (list 0 4 8 12)
        (list 1 5 9 13)
        (list 2 6 10 14)
        (list 3 7 11 15)))
(define BOXS
  (list (list  0 1 4 5)
        (list  2 3 6 7)
        (list  8 9 12 13)
        (list  10 11 14 15)))

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
                       

(define (find-row-temp index)
  (

(define (find-row index)
  (

;;当前局面位置可能放的值
(define (maybe-numbers index)
  (intersect (complement VALUES (find-box index))
             (intersect (complement VALUES (find-row index))
             (complement VALUES (find-row index)))))
  
  


;;数独求解器 list -> list or false
;(define (solve bd)
;  (if (full bd)
;          bd
;      (local [(define result  (solve-temp (maybe-number (first-box bd)) bd (first-box bd)))]
;        result)))
;
;(define (solve-temp maybe-numbers bd p)
;  (if (null? maybe-numbers)
;      false
;      (local [(define result (solve (fill-square bd p (car maybe-numbers))))]
;        (if (boolean? result)
;            (solve-temp (cdr maybe-numbers) bd p)
;            result))))
            
      
          
        
        
        
        


