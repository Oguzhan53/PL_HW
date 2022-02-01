
; Program can not control lists and control statements
; If you want to write input in commend line firts enter g++ than write your input
; If you want to read input in file enter g++ filename than program wil read file
; If you read file write a space line in file otherwise program give incorrect output 
   


(defun file-get-contents (filename) ;this function read file
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents
      )
    )
    
)

(defun end_id (index str) ;this function find id's end index
    (loop for i from index to (- (length str) 1) do
        (if  (string= (char str  i ) " ") 
            (return-from end_id i)
        )
        (if (= i (- (length str) 1) )
            (return-from end_id (+ i 1))
        )
    
    )

)

(defun initial_id (index str) ;this function find id's initial index
    (loop for i from index to (- (length str) 1) do
        (if  (string/= (char str  i ) " ") 
            (return-from initial_id i)
        )
        (if (= i (- (length str) 1) )
            (return-from initial_id (+ i 1))
        )
    
    )

)

(defun numeric-string (string) ;this function check string if it is numeric or not
    (let ((read-eval nil))
        (ignore-errors (numberp (read-from-string string)))
    )
)

(defun search_op ( op ) ;this function check char if it is op or not
    (setf op_type "+-*/")
    (loop for i from 0 to 3 do 
        (if (string= op (char op_type i) )
            (return-from search_op t)
            
        )
    )
    (return-from search_op nil)

)

(defun search_par (str in ) ;this function find close bracker
    (let ((par_num 1))
        (loop for i from (+ in 1) to (- (length str) 1) do 
            (if (string= (char str i) "(")
                (setf par_num (+ par_num 1))
            )
            (if (string= (char str i) ")")
                (setf par_num (- par_num 1))
            )
            (if (= par_num 0)
                (return-from search_par i)
            )
        )
        nil
    )
    


)


(defun index (str index opnum kwnum)  
    (let ( (kw nil) ( defnum 0)   ( l_num 0) (expi1 nil) (expi2 nil) )
  
    ; kw is the keyword type
    ; defnum is the deffun number
    ; opnum is the operator number
        (loop for c from index to (- (length str) 1) do

            (if  (>=  (- (length str) 1) (+ c 5)) 
                (progn
                
                    (if (and    (string-equal (subseq str c (+ c 6)) "deffun") ; this condition check deffun keyword
                                (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                                (or (string-equal (char str (+ c 6)) " ") (string-equal (char str (+ c 6)) "(") (string-equal (char str (+ c 6)) ")") )
                        )
                        
                        (progn
                            (if (or (trim_mid str "(" (+ c 6)) (trim_mid str ")" (+ c 6)) (trim_mid str "," (+ c 6) ) ) ;this condition check bracket
                                (progn
                                    
                                   
                                    (write-line "SYNTAX FALSE (BRACKET)")
                                    (setf resnum nil)
                                    (return)
                                )
                                (progn
                                    (if (= defnum 1)
                                        (progn
                                            
                                            (write-line "SYNTAX FALSE (MORE THAN 1 DEFFUN)")
                                            (setf resnum nil)
                                            (return)
                                        )
                                        
                                    )
                                    
                                )
                            )
                            (setf defnum 1)
                            (setf kw "defun")
                            
                        )
                        
                    )
                )
                (setf resnum 0)
            )
            
            (if (= l_num 0)
                (progn
                    (loop for c2 from index to (- (length str) 1) do
                        (if (string= (char str c2) "(") 
                            (progn
                                (setf c c2)
                                (return)
                            )
                        )
                    
                    )
                    (setf l_num 1)
                
                )
            
            )
            
            (if (and   (>=  (- (length str) 1) (+ c 1))   (search_op (subseq str c (+ c 1)) )    ;this condition chech operators
                    (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                    (or (string-equal (char str (+ c 1)) " ") (string-equal (char str (+ c 1)) "(") (string-equal (char str (+ c 1)) ")")  (search_op (subseq str  (+ c 1) (+ c 2) ) )    )
                )

                    
                    (progn
                       
                        (if (search_op (subseq str  (+ c 1) (+ c 2) ) ) 
                        (progn  
                               
                                (write-line "SYNTAX FALSE (MORE THAN 1 OP)")
                                (setf resnum nil)
                                (return)
                            )

                        )
                        (setf opnum 1)

                        (if (not (back_trim str "(" c))
                            (progn  
                               
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                            
                        )
                        (if (or  (trim_mid str ")" (+ c 1)) (trim_mid str "," (+ c 1) ) )
                            (progn
                                    
                                
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                        )
                        (setf kw (subseq str c (+ c 1)))
                        (if (= kwnum 1) 
                            (progn
                               
                                (write-line "SYNTAX FALSE (INCOMPATIBLE NESTED OP)")
                                (setf resnum nil)
                                (return)
                            )
                        )
                        (if (trim_mid str "(" (+ c 1))
                            (progn
                                (setf new_in (trim_mid_in str "(" (+ c 1)) )
                                (setf tmp (index str new_in opnum kwnum))
                                (if (equal tmp nil)
                                    (progn
                                        (setf resnum nil)
                                        (return) 
                                    )
                                    (setf expi1 (write-to-string tmp))
                                )
                                
                             
                                (setf c  (- (search_par str (trim_mid_in str "(" (+ c 1)) ) 1))
                            )
                            (progn
                                (setf expi1 (subseq str (initial_id (+ c 1) str) (end_id (initial_id (+ c 1) str) str )))
                            )
                            
                        )

                        (if (trim_mid str ")" (end_id (initial_id (+ c 1) str) str ))
                            (progn
                               
                                (write-line "SYNTAX FALSE (INCOMPATIBLE NESTED OP)")
                                (setf resnum nil)
                                (return)
                            )
                            (progn
                                (if (trim_mid str "(" (end_id (initial_id (+ c 1) str) str ))
                                    (progn  
                                        (setf new_in  (trim_mid_in str "(" (end_id (initial_id (+ c 1) str) str )))
                                        (setf tmp (index str new_in opnum kwnum))
                                        (if (equal tmp nil)
                                            (progn
                                                (setf resnum nil)
                                                (return) 
                                            )
                                            (setf expi2 (write-to-string tmp))
                                        )
                                        
                                        
                                        
                                    
                                    )
                                    (progn
                                        (setf expi2 (subseq str (initial_id (end_id (initial_id (+ c 1) str) str ) str) (end_id (initial_id (end_id (initial_id (+ c 1) str) str ) str) str ) ) )
                                        (if (or   (trim_mid str ")" (end_id (initial_id (end_id (initial_id (+ c 1) str) str ) str) str )) (string= (char expi2 (- (length expi2) 1)) ")") )
                                            (progn
                                                (if (or (string= (char expi2 (- (length expi2) 1)) ")") (string= (char expi2 (- (length expi2) 1)) "(")  )
                                                    (progn
                                                        (setf (char expi2 (- (length expi2) 1)) #\Space )
                                                    )
                                                        
                                                )
                                            )
                                            (progn
                                                
                                                (write-line "SYNTAX FALSE (MORE THAN 2 NUMBER)")
                                                (setf resnum nil)
                                                (return)
                                            )
                                        )
                                    )
                                )
                            
                            )
                            
                            
                        )

                        
                        
                        
                    
               
                        (if (and (numeric-string expi1) (numeric-string expi2))
                            (progn
                                
                                (setf expi1 (parse-integer expi1))
                                (setf expi2 (parse-integer expi2))
                                (if (string= kw "+")
                                    (progn
                                        (setf resnum (+ expi1 expi2))
                                    )
                                )
                                (if (string= kw "-")
                                    (progn 

                                        (setf resnum (- expi1 expi2))
                                       
                                    )
                                )
                                (if (string= kw "*")
                                    (progn
                                        (setf resnum (* expi1 expi2))
                                    )
                                )
                                (if (string= kw "/")
                                    (progn
                                        (setf resnum (/ expi1 expi2))
                                    )
                                )
                                (return)
                                
                                
                            )


                        )
                        
                    
                    

        
                )
                
            
            
            )


            
    
            (if (and   (>=  (- (length str) 1) (+ c 3))  (or  (string-equal (subseq str c (+ c 3)) "and") (string-equal (subseq str c (+ c 3)) "not")  ) ;this condition chek "and" and "not" keyword 
                    (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                    (or (string-equal (char str (+ c 3)) " ") (string-equal (char str (+ c 3)) "(") (string-equal (char str (+ c 3)) ")") )
                )
                (progn
                    (if (not (back_trim str "(" c))
                        (progn  
                            
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                        
                    )
                    (if (or  (trim_mid str ")" (+ c 3)) (trim_mid str "," (+ c 3) ) )
                        (progn
                                
                           
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                    )
                    (setf kwnum 1)
                    (if (= opnum 1)
                        (progn
                           
                            (write-line "SYNTAX FALSE (INCOMPATIBLE NESTED OP)")
                            (setf resnum nil)
                            (return)
                        
                        
                        )
                    )
                    (if (string-equal (subseq str c (+ c 3)) "and")
                        (progn 
                            (setf kw "and")

                            (if (trim_mid str "(" (+ c 3))
                                (progn
                                    (setf new_in (trim_mid_in str "(" (+ c 3)) )
                                    (setf tmp (index str new_in opnum kwnum))
                                    (if (equal tmp nil)
                                        (progn
                                            (setf resnum nil)
                                            (return) 
                                        )
                                        (setf expi1 (write-to-string tmp))
                                    )

                               
                                    (setf c  (- (search_par str (trim_mid_in str "(" (+ c 3)) ) 3))
                                )
                                (progn
                                    (setf expi1 (subseq str (initial_id (+ c 3) str) (end_id (initial_id (+ c 3) str) str )))
                                )
                                
                            )

                            (if (trim_mid str ")" (end_id (initial_id (+ c 3) str) str ))
                                (progn
                                   
                                    (write-line "SYNTAX FALSE (BRACKET)")
                                    (setf resnum nil)
                                    (return)
                                )
                                (progn
                                    (if (trim_mid str "(" (end_id (initial_id (+ c 3) str) str ))
                                        (progn  
                                            (setf new_in  (trim_mid_in str "(" (end_id (initial_id (+ c 3) str) str )))
                                            
                                            (setf tmp (index str new_in opnum kwnum))
                                            (if (equal tmp nil)
                                                (progn
                                                    (setf resnum nil)
                                                    (return) 
                                                )
                                                (setf expi2 (write-to-string tmp))
                                            )

                                            
                                            
                                        
                                        )
                                        (progn
                                            (setf expi2 (subseq str (initial_id (end_id (initial_id (+ c 3) str) str ) str) (end_id (initial_id (end_id (initial_id (+ c 3) str) str ) str) str ) ) )
                                            (if (or   (trim_mid str ")" (end_id (initial_id (end_id (initial_id (+ c 3) str) str ) str) str )) (string= (char expi2 (- (length expi2) 1)) ")") )
                                                (progn
                                                    (if (or (string= (char expi2 (- (length expi2) 1)) ")") (string= (char expi2 (- (length expi2) 1)) "(")  )
                                                        (progn
                                                            (setf (char expi2 (- (length expi2) 1)) #\Space )
                                                        )
                                                            
                                                    )
                                                )
                                                (progn
                                                   
                                                    (write-line "SYNTAX FALSE (MORE THAN 2 NUMBER)")
                                                    (setf resnum nil)
                                                    (return)
                                                )
                                            )
                                        )
                                    )
                                
                                )
                                
                                
                            )

                            
                            
                           
                        
                      
                            (if (and (numeric-string expi1) (numeric-string expi2))
                                (progn
                                    
                                    (if (and (or (= (parse-integer expi1) 0 )  (= (parse-integer expi1) 1) ) (or (= (parse-integer expi2) 0 )  (= (parse-integer expi2) 1) )  )
                                        (progn

                                            (if (and (= (parse-integer expi1) 1) (= (parse-integer expi2) 1) )
                                                (setf resnum 1)
                                                (setf resnum 0)
                                            )
                                            
                                            
                                            (return)
                                        )
                                        (progn
                                            
                                            (write-line "SYNTAX FALSE (NOT BINARY)")
                                            (setf resnum nil)
                                            (return)
                                        )
                                    )   
                                    
                                )
                                (progn
                                   
                                    (write-line "SYNTAX FALSE (NOT NUMBER)")
                                    (setf resnum nil)
                                    (return)
                                )

                            )
                            
                        
                        

                        )
                        (progn 
                            (setf kw "not")

                            (if (trim_mid str "(" (+ c 3))
                                (progn
                                    (setf new_in (trim_mid_in str "(" (+ c 3)) )

                                    (setf tmp (index str new_in opnum kwnum))
                                    (if (equal tmp nil)
                                        (progn
                                            (setf resnum nil)
                                            (return) 
                                        )
                                        (setf expi1 (write-to-string tmp))
                                    )


                                    
                                    (setf c  (- (search_par str (trim_mid_in str "(" (+ c 3)) ) 3))
                                )
                                (progn
                                    (setf expi1 (subseq str (initial_id (+ c 3) str) (end_id (initial_id (+ c 3) str) str )))
                                )
                                
                            )


                            (if (or  (trim_mid str ")" (end_id (initial_id (+ c 3) str) str )  ) (string= (char expi1 (- (length expi1) 1)) ")")  )
                                (progn
                                    (if  (string= (char expi1 (- (length expi1) 1)) ")")  
                                        (progn
                                            (setf (char expi1 (- (length expi1) 1)) #\Space )
                                        )
                                        
                                    )

                                    (if  (numeric-string expi1) 
                                        (progn
                                            
                                            (if  (or (= (parse-integer expi1) 0 )  (= (parse-integer expi1) 1) ) 
                                                (progn 
                                                    (if (= (parse-integer expi1) 1)
                                                        (setf resnum 0)
                                                        (setf resnum 1)
                                                    )
                                                    
                                                    (return)
                                                )
                                                (progn
                                                  
                                                    (write-line "SYNTAX FALSE (NOT BINARY)")
                                                    (setf resnum nil)
                                                    (return)
                                                )
                                            )   
                                            
                                        )
                                        (progn
                                           
                                            (write-line "SYNTAX FALSE (NOT NUMBER)")
                                            (setf resnum nil)
                                            (return)
                                        )

                                    )
                                        
                                        
                                
                                )
                                (progn
                                    
                                    (write-line "SYNTAX FALSE (MORE THAN 1 NUMBER)")
                                    (setf resnum nil)
                                    (return)
                
                                )
                            )
                            
                        
                        )

                    )

                    
                    


                )
                

                
            
            )

            (if (and   (>=  (- (length str) 1) (+ c 2))   (string-equal (subseq str c (+ c 2)) "or")  ;this condition check or condition  
                    (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                    (or (string-equal (char str (+ c 2)) " ") (string-equal (char str (+ c 2)) "(") (string-equal (char str (+ c 2)) ")") )
                )
                    (progn
                        (if (not (back_trim str "(" c))
                            (progn  
                               
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                            
                        )
                        (if (or  (trim_mid str ")" (+ c 2)) (trim_mid str "," (+ c 2) ) )
                            (progn
                                    
                               
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                        )
                        (setf kw "or")
                        (setf kwnum 1)
                        (if (= opnum 1)
                            (progn
                                
                                (write-line "SYNTAX FALSE (INCOMPATIBLE NESTED OP)")
                                (setf resnum nil)
                                (return)
                            
                            
                            )
                        )   

                        (if (trim_mid str "(" (+ c 2))
                            (progn
                                (setf new_in (trim_mid_in str "(" (+ c 2)) )

                                (setf tmp (index str new_in opnum kwnum))
                                (if (equal tmp nil)
                                    (progn
                                        (setf resnum nil)
                                        (return) 
                                    )
                                    (setf expi1 (write-to-string tmp))
                                )

                                
                                ; (print expi1)
                                (setf c  (- (search_par str (trim_mid_in str "(" (+ c 2)) ) 2))
                            )
                            (progn
                                (setf expi1 (subseq str (initial_id (+ c 2) str) (end_id (initial_id (+ c 2) str) str )))
                            )
                            
                        )

                        (if (trim_mid str ")" (end_id (initial_id (+ c 2) str) str ))
                            (progn
                              
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                            (progn
                                (if (trim_mid str "(" (end_id (initial_id (+ c 2) str) str ))
                                    (progn  
                                        (setf new_in  (trim_mid_in str "(" (end_id (initial_id (+ c 2) str) str )))
                                        
                                        (setf tmp (index str new_in opnum kwnum))
                                        (if (equal tmp nil)
                                            (progn
                                                (setf resnum nil)
                                                (return) 
                                            )
                                            (setf expi2 (write-to-string tmp))
                                        )
                                        
                                        
                                        
                                        
                                    
                                    )
                                    (progn
                                        (setf expi2 (subseq str (initial_id (end_id (initial_id (+ c 2) str) str ) str) (end_id (initial_id (end_id (initial_id (+ c 2) str) str ) str) str ) ) )
                                        (if (or   (trim_mid str ")" (end_id (initial_id (end_id (initial_id (+ c 2) str) str ) str) str )) (string= (char expi2 (- (length expi2) 1)) ")") )
                                            (progn
                                                (if (or (string= (char expi2 (- (length expi2) 1)) ")") (string= (char expi2 (- (length expi2) 1)) "(")  )
                                                    (progn
                                                        (setf (char expi2 (- (length expi2) 1)) #\Space )
                                                    )
                                                        
                                                )
                                            )
                                            (progn
                                                
                                                (write-line "SYNTAX FALSE (MORE THAN 2 NUMBER)")
                                                (setf resnum nil)
                                                (return)
                                            )
                                        )
                                    )
                                )
                            
                            )
                            
                            
                        )

                        
                        
                        
                    
                    
                        (if (and (numeric-string expi1) (numeric-string expi2))
                            (progn
                                
                                (if (and (or (= (parse-integer expi1) 0 )  (= (parse-integer expi1) 1) ) (or (= (parse-integer expi2) 0 )  (= (parse-integer expi2) 1) )  )
                                    (progn

                                        (if (and (= (parse-integer expi1) 0) (= (parse-integer expi2) 0) )
                                            (setf resnum 0)
                                            (setf resnum 1)
                                        )
                                        
                                        
                                        (return)
                                    )
                                    (progn
                                        
                                        (write-line "SYNTAX FALSE (NOT BINARY)")
                                        (setf resnum nil)
                                        (return)
                                    )
                                )   
                                
                            )
                            (progn
                                
                                (write-line "SYNTAX FALSE (NOT NUMBER)")
                                (setf resnum nil)
                                (return)
                            )

                        )
                        
                    
                    

        
                )
                
            
            
            )

            (if (and   (>=  (- (length str) 1) (+ c 5))   (string-equal (subseq str c (+ c 5)) "equal")    ; this condition check equl keyword
                    (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                    (or (string-equal (char str (+ c 5)) " ") (string-equal (char str (+ c 5)) "(") (string-equal (char str (+ c 5)) ")") )
                )
                (progn
                    (if (not (back_trim str "(" c))
                        (progn  
                           
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                        
                    )
                    (if (or  (trim_mid str ")" (+ c 2)) (trim_mid str "," (+ c 2) ) )
                        (progn
                                
                          
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                    )
                    (setf kw "equal")
                    (setf kwnum 1)
                    (if (trim_mid str "(" (+ c 5))
                            (progn
                                (setf new_in (trim_mid_in str "(" (+ c 5)) )

                                (setf tmp (index str new_in opnum kwnum))
                                (if (equal tmp nil)
                                    (progn
                                        (setf resnum nil)
                                        (return) 
                                    )
                                    (setf expi1 (write-to-string tmp))
                                )


                                
                                ; (print expi1)
                                (setf c  (- (search_par str (trim_mid_in str "(" (+ c 5)) ) 5))
                            )
                            (progn
                                (setf expi1 (subseq str (initial_id (+ c 5) str) (end_id (initial_id (+ c 5) str) str )))
                            )
                            
                        )

                        (if (trim_mid str ")" (end_id (initial_id (+ c 5) str) str ))
                            (progn
                               
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                            (progn
                                (if (trim_mid str "(" (end_id (initial_id (+ c 5) str) str ))
                                    (progn  
                                        (setf new_in  (trim_mid_in str "(" (end_id (initial_id (+ c 5) str) str )))
                                        
                                        (setf tmp (index str new_in opnum kwnum))
                                        (if (equal tmp nil)
                                            (progn
                                                (setf resnum nil)
                                                (return) 
                                            )
                                            (setf expi2 (write-to-string tmp))
                                        )
                                        
                                     
                                        
                                    
                                    )
                                    (progn
                                        (setf expi2 (subseq str (initial_id (end_id (initial_id (+ c 5) str) str ) str) (end_id (initial_id (end_id (initial_id (+ c 5) str) str ) str) str ) ) )
                                        (if (or   (trim_mid str ")" (end_id (initial_id (end_id (initial_id (+ c 5) str) str ) str) str )) (string= (char expi2 (- (length expi2) 1)) ")") )
                                            (progn
                                                (if (or (string= (char expi2 (- (length expi2) 1)) ")") (string= (char expi2 (- (length expi2) 1)) "(")  )
                                                    (progn
                                                        (setf (char expi2 (- (length expi2) 1)) #\Space )
                                                    )
                                                        
                                                )
                                            )
                                            (progn
                                                
                                                (write-line "SYNTAX FALSE (MORE THAN 2 NUMBER)")
                                                (setf resnum nil)
                                                (return)
                                            )
                                        )
                                    )
                                )
                            
                            )
                            
                            
                        )

                        
                        
                        
                    
                    
                    (setf  expi2 (string-right-trim " " expi2))
                    (if (string= expi2 expi1)
                        (setf resnum 1)
                        (setf resnum 0)
                    
                    )
                    (return)
                        
                    
                )
            
            
            )









            (if (and   (>=  (- (length str) 1) (+ c 3))   (string-equal (subseq str c (+ c 3)) "set")    ; this condition check equl keyword
                    (or (string-equal (char str (- c 1)) "(") (string-equal (char str (- c 1) ) " ") )
                    (or (string-equal (char str (+ c 3)) " ") (string-equal (char str (+ c 3)) "(") (string-equal (char str (+ c 3)) ")") )
                )
                (progn
                    (if (not (back_trim str "(" c))
                        (progn  
                           
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                        
                    )
                    (if (or  (trim_mid str ")" (+ c 3)) (trim_mid str "," (+ c 3) ) )
                        (progn
                                
                          
                            (write-line "SYNTAX FALSE (BRACKET)")
                            (setf resnum nil)
                            (return)
                        )
                    )
                    (setf kw "set")
                    
                    (if (trim_mid str "(" (+ c 3))
                            (progn
                                (setf new_in (trim_mid_in str "(" (+ c 3)) )

                                (setf tmp (index str new_in opnum kwnum))
                                (if (equal tmp nil)
                                    (progn
                                        (setf resnum nil)
                                        (return) 
                                    )
                                    (setf expi1 (write-to-string tmp))
                                )


                                
                                ; (print expi1)
                                (setf c  (- (search_par str (trim_mid_in str "(" (+ c 3)) ) 3))
                            )
                            (progn
                                (setf expi1 (subseq str (initial_id (+ c 3) str) (end_id (initial_id (+ c 3) str) str )))
                            )
                            
                        )

                        (if (trim_mid str ")" (end_id (initial_id (+ c 3) str) str ))
                            (progn
                               
                                (write-line "SYNTAX FALSE (BRACKET)")
                                (setf resnum nil)
                                (return)
                            )
                            (progn
                                (if (trim_mid str "(" (end_id (initial_id (+ c 3) str) str ))
                                    (progn  
                                        (setf new_in  (trim_mid_in str "(" (end_id (initial_id (+ c 3) str) str )))
                                        
                                        (setf tmp (index str new_in opnum kwnum))
                                        (if (equal tmp nil)
                                            (progn
                                                (setf resnum nil)
                                                (return) 
                                            )
                                            (setf expi2 (write-to-string tmp))
                                        )
                                        
                                     
                                        
                                    
                                    )
                                    (progn
                                        (setf expi2 (subseq str (initial_id (end_id (initial_id (+ c 3) str) str ) str) (end_id (initial_id (end_id (initial_id (+ c 3) str) str ) str) str ) ) )
                                       
                                        (if (or   (trim_mid str ")" (end_id (initial_id (end_id (initial_id (+ c 3) str) str ) str) str )) (string= (char expi2 (- (length expi2) 1)) ")") )
                                            (progn
                                                (if (or (string= (char expi2 (- (length expi2) 1)) ")") (string= (char expi2 (- (length expi2) 1)) "(")  )
                                                    (progn
                                                        (setf (char expi2 (- (length expi2) 1)) #\Space )
                                                    )
                                                        
                                                )
                                            )
                                            (progn
                                                
                                                (write-line "SYNTAX FALSE (MORE THAN 2 NUMBER)")
                                                (setf resnum nil)
                                                (return)
                                            )
                                        )
                                    )
                                )
                            
                            )
                            
                            
                        )

                        
                        
                        
                    
                    
                    (setf  expi2 (string-right-trim " " expi2))
                    
                   
                    (if (numeric-string expi1)
                        (progn
                            (write-line "SYNTAX FALSE (NO ID)")
                            (setf resnum nil)
                        
                        )
                        (setf resnum expi2)
                    
                    )
                    (return)
                        
                    
                )
            
            
            )








        
        )
        
      
        resnum
    )
)









(defun trim_mid (str ch c) ;this function check before space character.
    (setf v " ")
    (loop for i from c to (- (length str) 1) do 
       
        (if (or  (string/= (char str i) " ") (string= (char str i) #\Newline)) 
            (progn 
                
                (setf v (char str i))
                (return )
            )
               
        )

          (if (string= ch ",") ; if you give ',' parameter function check end of the id
            (if (= i (- (length str) 1) )
                (progn
                    (return-from trim_mid t)
                    
                )
                
            )
        )
    )

    (if (string= v ch)
        (return-from trim_mid t)
        (return-from trim_mid nil)
    )


)


(defun trim_mid_in (str ch c) ;this function return before space character index
    (setf v " ")
    (loop for i from c to (- (length str) 1) do 
    (setf in nil)
        (if (or  (string/= (char str i) " ") (string= (char str i) #\Newline)) 
            (progn 
                
                (setf v (char str i))
                (setf in i)
                (return )
            )
               
        )

          (if (string= ch ",") ;  if you give ',' parameter function check end of the id
            (if (= i (- (length str) 1) )
                (progn
                    (return-from trim_mid_in (+ i 1))
                    
                )
                
            )
        )
    )

    (if (string= v ch)
        (return-from trim_mid_in in)
        (return-from trim_mid_in nil)
    )


)



(defun back_trim (str ch c) ;this function find the id's initial
    (setf v " ")
    (setf c (- c 1))
    (loop for i from c downto 0 do
         (if (string/= (char str i) " ")
            (progn 
                (setf v (char str i))
                (return )
            )
               
        )
    )
    (if (string= v ch)
        t
        nil
    )

)



(defun gppinterpreter ()

    
    (setf rstr (read-line))
    (setf rstr (string-left-trim " " rstr))
    (setf result 1)
    (if (>=  (length rstr) 5 )
        (progn
            (loop for i from 0 to (- (length rstr) 1) do 
                (if (and (string= (char rstr i) " ") (string/= (char rstr (+ i 1)) " ")) ;find new line number
                    (progn 
                        (setf i1 (end_id (+ i 1) rstr))
                        (setf rstr (subseq rstr (+ i 1) i1))
                        (return)
                    )
                )
            
            )
            (setf str (file-get-contents rstr))


            (setf line_num 0)
            (setf b 0)
            (loop for i from 0 to  (- (length str) 1)  do 
                (setf l 0)
                (if  (string= (char str i) #\Newline)
                      
                    (progn
                        (setf try (subseq str b i))
                        (setf b (+ i 1))
                        (setf l 1)
                        
                        
                    )
                    (progn
                        (if (= i (- (length str) 1) ) 
                            (progn
                                (setf try (subseq str b (- i 1)))
                                (setf l 1)
                                
                                
                            )
                        )
                    
                    )
                    
                    
                )
                (if (= l 1)
                    (progn
                        (setf l1 0)
                        (loop for i1 from 0 to (- (length try ) 1) do
                            (if (or (string/= (char try i1 ) " ") (string= (char try i1 ) #\Newline) )
                                (progn
                                    (setf l1 1)
                                    (return)
                                )
                            )
                        
                        )

                        (if (= l1 1)
                        
                            (progn

                                    (setf en (trim_mid try "(" 0))
                                    (if (equal en t)
                                        (progn
                                            
                                            (setf result (index try 0 0 0))
                                            
                                            
                                        )
                                        
                                        (progn
                                            
                                            (setf result nil)
                                        )

                                    )
                                    (if (not (equal result nil))
                                        (progn
                                            (write-line "SYNTAX OK")
                                            (format t "Result : ~D ~%" result)
                                        
                                        )
                                        (return)
                                        
                                    
                                    )
                            
                            
                            )
                        
                        )
                        
                    
                    
                    )
                )
            
            )
           ; (print line_num)
        
        
        )
        (progn
            (setf str (read-line))

            (setf en (trim_mid str "(" 0))
            (if (equal en t)
                (setf result (index str 0 0 0))
                (progn
                (write-line "syntax error")
                )

            )
            (if (not (equal result nil))
                (progn
                    (write-line "SYNTAX OK")
                    (format t "Result : ~D ~%" result)
                
                )
                
            
            )



        
        )
    
    )
    
    
    
    ;(print (search_op "**"))

)   

(gppinterpreter)

