; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************



;; **********************************************************************************   			
;; *         OGUZHAN SEZGIN - 1801042005-                                           *
;; *                                                                                *
;; *		I try six letter.You can expand plain and cheaper text for test case	* 
;; * 	but decoding time may be extand.There is no Gen-Decoder-B-1.				*
;; *																				*
;; *																				*
;; **********************************************************************************


(defun read-as-list (filename) ;This function read file character by character and make nested list.
	(let ((contents))
		(with-open-file (stream filename)
			(let ((contents (make-list (file-length stream))))
			(read-sequence contents stream)
			contents
			(make-nested-list contents)
			)
			
		)
	)
)
(defun file-get-lines (filename) ;This function read dictionary file line by line
    (let ()
        (with-open-file (stream filename)
            (loop for line = (read-line stream nil)
            while line
            collect line
            )
        )
    )
)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***
(defun make-nested-list (my-list);This function convert normal list to nested list. 
	
	(let ((c 0) (c1 0) (lst1 (make-list 0 :initial-element(make-list 1 ))) (lst2 (make-list 0 :initial-element(make-list 1 ))))
		(dotimes (x  (+ (list-length my-list) 1)) 
			(if  (or  (equal (nth x my-list)  #\space) (equal  x (list-length my-list))  (equal (nth x my-list)  #\Newline) )
				(progn 
					
					(setq  lst1 (append lst1 (list lst2 )))
					(setf lst2 nil)
					(if (equal  x (list-length my-list))
						(return ) 
					)					
				)
				(progn 
					(setf  lst2 (append  lst2 (list (nth x my-list))))
					(setf lst2 (delete nil lst2))
				)
				
				
			)
		)
		
		(setf lst1 (delete nil lst1))
		(return-from make-nested-list lst1)

	)

)
(defun swap-list (lst n1 n2)
    (let ((n3 (nth n1 lst)))
        (setf (nth n1 lst) (nth n2 lst))
        (setf (nth n2 lst) n3)
    )
    
)        
(defun decode (p-word cp-lst pl-lst) ;This function decode words according to cheaper alphabet   
    (let ((word nil))
        (loop for pr-ch from 0 to (- (list-length p-word) 1) do 
                  
            (loop for cp from 0 to (- (list-length cp-lst) 1) do
                (setf x (nth cp cp-lst))
                (setf x (format nil "~(~a~)" x))
                (if (string= (nth pr-ch p-word) x )
                    (progn
                        
                    (setf word (append word (list (nth cp pl-lst))))
                        
                    )                            
                )
            )
        )
       (return-from decode word) 
    )
)
(defun string-to-list (str)
    (let ()
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil
            )
        )
        
    )              
)
(defun create-hash (dict-words) ;This funtion create hash table for spell_checker_1
    (let ( (hash-dict))
        (setf hash-dict (make-hash-table))
        (dotimes (x (list-length dict-words) )
            (setf n (string-to-list(nth x dict-words)))
            (setf (gethash (car n) hash-dict) t)
        )
        (return-from create-hash hash-dict) 
    )
)
(defun find-possibility (dictionary cp-text paragraph pl-lst count size key e-freq t-freq a-freq o-freq i-freq n-freq) 
    (let ((stop nil) (sti (make-list 6 :initial-element 'f)) (all nil) (stuation t) )

        (if (and (= count size) (equal (car key) 'b))
            (progn
                (loop for p from 0 to (- (list-length pl-lst) 1) do  ;This loop check mapping is suitable for frequency  analysis
                    (if (equalp (nth p pl-lst) 'e)
                        (progn
                            (dolist (e e-freq)
                                (if (equalp e (nth p cp-text) )    
                                    (progn
                                        (setf (nth 0 sti) t)
                                    )
                                )
                                        
                            )
                        )
                        
                    )
                    (if (equalp (nth p pl-lst) 't)
                        (progn
                            (dolist (e t-freq)
                                (if (equalp e (nth p cp-text) )
                                    (progn
                                        (setf (nth 1 sti) t)
                                    )
                                ) 
                            )
                        )
                    )
                    (if (equalp (nth p pl-lst) 'a)
                        (progn
                            (dolist (e a-freq)
                                (if (equalp e (nth p cp-text) )
                                    (setf (nth 2 sti) t)
                                )   
                            )
                        )
                    )
                    (if (equalp (nth p pl-lst) 'o)
                        (progn
                            (dolist (e o-freq)
                                (if (equalp e (nth p cp-text) )
                                    (setf (nth 3 sti) t)  
                                ) 
                            )
                        )
                    )
                    (if (equalp (nth p pl-lst) 'i)
                        (progn
                            (dolist (e i-freq)
                                (if (equalp e (nth p cp-text) )
                                    (setf (nth 4 sti) t)  
                                )   
                            )
                        )
                    )
                    (if (equalp (nth p pl-lst) 'n)
                        (progn
                            (dolist (e n-freq)
                                (if (equalp e (nth p cp-text) )
                                    (setf (nth 5 sti) t)
                                )        
                            )
                        )
                    )

                    
                )
                (dolist (s sti)
                    (if (equal s 'f)
                        (progn
                            (setf stop t)
                        )   
                    )        
                )
                (if (not (equalp stop t)) ;If it is suitable for frequency  analysis ,program will decode input words 
                    (progn
                        (loop for pr from 0 to (- (list-length paragraph) 1) do
                            (setf word (decode (nth pr paragraph) cp-text pl-lst))
                            (setf stuation (spell-checker-0 word dictionary))
                            
                        
                            
                            
                            (if (not (equal stuation t));if all words in document are not occurs in dictionary this cheaper alphabet is not true
                                (progn
                                    (return )
                                )
                                
                            )  
                        )
                        (if (equal stuation t);if all words in document are occurs in dictionary situation will true and decode words according to this cheaper alphabet. 
                            (progn 
                                (loop for ec from 0 to (- (list-length paragraph) 1) do 
                                    (format t "~a  " (decode (nth ec paragraph) cp-text pl-lst))
                                )
                                (setf (car key) 'a) ;if all words is true , it's not necessary to search other mapping option.
                            )
                        )   
                    
                    
                    )
                )
                
                
            )
            (progn
                (if (and (not (= count size)) (equal (car key) 'b)) ;key=a means that program find true cheaper alphabet and no need search other one.
                    (progn
                        (loop  for c1 from count to size do 
                            
                            (swap-list cp-text count c1)
                            (find-possibility dictionary cp-text paragraph pl-lst (+ count 1) size key e-freq t-freq a-freq o-freq i-freq n-freq )
                            (swap-list cp-text count c1)
                        )
                    )
                )
            )
        )
    )
    
    
)


(defun spell-checker-0 (word dictionary)
    (let () 
        (loop for dc in dictionary do
            (if (equalp dc (format nil "~{~A~}" word))
                (progn
                    (return-from spell-checker-0 t)
                )
                        
            )
        )
    
    )    
  
)

(defun spell-checker-1 (word hash-dict)
    (let () 
        (setf word (format nil "~{~a~}" word))
        (setf word (string-to-list word))
        (if (gethash (car word) hash-dict)
            (return-from spell-checker-1 t)
        )
    )
    

)


;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen_Decoder_A (dictionary cp-lst paragraph pl-lst count size key hash-dict option)   
    (let ((all nil) (stuation t))
        
        (if (and (= count size) (equal (car key) 'b))
            (progn
                (loop for pr from 0 to (- (list-length paragraph) 1) do
                    (setf word (decode (nth pr paragraph) cp-lst pl-lst))
                    (if (= option 0)
                        (setf stuation (spell-checker-0 word dictionary))
                    )
                    (if (= option 1)
                        (setf stuation (spell-checker-1 word hash-dict))
                    )
                    
                    
                    (if (not (equal stuation t));if all words in document are not occurs in dictionary this cheaper alphabet is not true
                        (progn
                            (return )
                        )
                        
                    )  
                )
                (if (equal stuation t);if all words in document are occurs in dictionary situation will true and decode words according to this cheaper alphabet. 
                    (progn 
                        (loop for ec from 0 to (- (list-length paragraph) 1) do 
                            (format t "~a  " (decode (nth ec paragraph) cp-lst pl-lst))
                        )
                        (setf (car key) 'a) ;if all words is true , it's not necessary to search other mapping option.
                    )
                )  
            )
            (progn
                (if (and (not (= count size)) (equal (car key) 'b)) ;key=a means that program find true cheaper alphabet and no need search other one.
                    (progn
                        (loop  for c1 from count to size do 
                            
                            (swap-list cp-lst count c1)
                            (Gen_Decoder_A dictionary cp-lst paragraph pl-lst (+ count 1) size key hash-dict option)
                            (swap-list cp-lst count c1)
                        )
                    )
                )
            )
        )

    )
    
    
    
)

(defun Gen_Decoder_B_0 (paragraph cp-text dictionary  pl-lst count size key  )
    
    (let ((let-num '()) (e-freq '()) (t-freq '()) (a-freq '()) (o-freq '()) (i-freq '()) (n-freq '()) (big 0) (mst-index 0) (st-index 5) (big1))

        (dotimes (n (list-length cp-text))
            (setf let-num (append let-num (list 0)))
        )

        (dolist (pr paragraph)  ;this loop find number of letter and store them in let-num
            (dolist (pr-word pr)
                (loop for cp from 0 to (- (list-length cp-text) 1) do
                    (setf x (nth cp cp-text))
                    (setf x (format nil "~(~a~)" x))    
                    (if (string= x pr-word)
                    (progn
                            (setf (nth cp let-num) (+ (nth cp let-num) 1))
                            (if (>= (nth cp let-num) big)
                                (setf big (nth cp let-num))
                            )
                    )
                        
                    )
                ) 
            )
            
        )
        
        (dolist (n cp-text)  ;This loop put cheaper letter in the can be replaced letter lists.
            (setf big1 0)
            (loop for lt from 0 to (- (list-length let-num) 1) do 
                (if (>= (nth lt let-num) big1)
                    (progn
                        (setf big1 (nth lt let-num))
                        (setf mst-index lt)
                    )
                    
                )
            )
            (setf (nth mst-index let-num) -1)

            (if (< big1 big)
                (progn
                    (setf big big1)
                    (setf st-index (- st-index 1))
                )
                
            )
            (if (and (= st-index 5) (not (= big 0)))
                (progn
                    (setf e-freq (append e-freq (list (nth mst-index cp-text))))
                    (setf t-freq (append t-freq (list (nth mst-index cp-text))))
                    (setf a-freq (append a-freq (list (nth mst-index cp-text))))
                    (setf o-freq (append o-freq (list (nth mst-index cp-text))))
                    (setf i-freq (append i-freq (list (nth mst-index cp-text))))
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )

            (if (and (= st-index 4) (not (= big 0)))
                (progn
                    (setf t-freq (append t-freq (list (nth mst-index cp-text))))
                    (setf a-freq (append a-freq (list (nth mst-index cp-text))))
                    (setf o-freq (append o-freq (list (nth mst-index cp-text))))
                    (setf i-freq (append i-freq (list (nth mst-index cp-text))))
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )
            (if (and (= st-index 3) (not (= big 0)))
                (progn
                    (setf a-freq (append a-freq (list (nth mst-index cp-text))))
                    (setf o-freq (append o-freq (list (nth mst-index cp-text))))
                    (setf i-freq (append i-freq (list (nth mst-index cp-text))))
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )
            (if (and (= st-index 2) (not (= big 0)))
                (progn
                    (setf o-freq (append o-freq (list (nth mst-index cp-text))))
                    (setf i-freq (append i-freq (list (nth mst-index cp-text))))
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )
            (if (and (= st-index 1) (not (= big 0)))
                (progn
                    (setf i-freq (append i-freq (list (nth mst-index cp-text))))
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )
            (if (and (= st-index 0) (not (= big 0)))
                (progn
                    (setf n-freq (append n-freq (list (nth mst-index cp-text))))
                    (setf big1 0)
                )
            )


        )

        (find-possibility dictionary cp-text paragraph pl-lst count size key e-freq t-freq a-freq o-freq i-freq n-freq )

        )
    
    ; let-num  This list store number of letter which in input data
    ; e-freq   This list store chiper letters that can be replaced with the letter e according to frequency  analysis
    ; t-freq   This list store chiper letters that can be replaced with the letter t according to frequency  analysis
    ; a-freq   This list store chiper letters that can be replaced with the letter a according to frequency  analysis.
    ; o-freq   This list store chiper letters that can be replaced with the letter o according to frequency  analysis
    ; i-freq   This list store chiper letters that can be replaced with the letter i according to frequency  analysis
    ; n-freq   This list store chiper letters that can be replaced with the letter n according to frequency  analysis

    


)

(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

(defun Code-Breaker (document decoder)
  	;you should implement this function
)

;; -----------------------------------------------------
;; Test code...


(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
    (format t "~%")
    (let ((plain-text '(e t a o i n)) (cheaper-text '(a t i f j k)) (paragraph (read-as-list "document1.txt")) (dict-words (file-get-lines "dictionary1.txt"))
     (hash-dict) (size) (key '(b)) (menu 0) (option 1) ) 
        (setf size (- (list-length cheaper-text) 1))
		(if (= menu 0) ;if menu=0 and option=0 program will use brute force method,if menu=0 and option=1 program use hash mapping method,if menu=1 program use frequency  analysis method  
			(progn
				(if (= option 0)
					(Gen_Decoder_A dict-words cheaper-text paragraph plain-text 0 size key hash-dict option)
					(progn
						(setf hash-dict (create-hash dict-words))
						(Gen_Decoder_A dict-words cheaper-text paragraph plain-text 0 size key hash-dict option)
					)
				)
			)
			(Gen_Decoder_B_0 paragraph cheaper-text dict-words plain-text 0 size key)
		)
        
        
        
    )
    

   
)



(test_on_test_data)