#
# Define the Fibonacci function
#
(def fib (N)
  (?: (<= N 1)
    N
    (+ (fib (- N 1)) (fib (- N 2)))
    ))

#
# Call the Fibonacci function
#
(prinl "Result: " (fib 30))
