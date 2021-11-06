(add-to-load-path (dirname (current-filename)))

(use-modules (test-driver))
(use-modules (compiler x86))

(clear-tests)

(add-tests-with-string-output "integers"      
  [0  -> "0\n"]                    
  [1  -> "1\n"]                    
  [-1 -> "-1\n"]                   
  [10  -> "10\n"]                    
  [-10 -> "-10\n"]                   
  [2736 -> "2736\n"]               
  [-2736 -> "-2736\n"]             
  [536870911 -> "536870911\n"]     
  [-536870912 -> "-536870912\n"]   
)

(display-tests)

(compile #t "build/stst.s")
