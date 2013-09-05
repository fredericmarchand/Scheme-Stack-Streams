Frederic Marchand
ID# 100817579
Comp3007
Assignment 3
Prof. Franz Oppacher

I used DrRacket to do this assignment as Prof. Oppacher has told the class to do.

All questions are answered in their separate racket file.

Comments have been added next to each test in the code section and the expected output of each test is printed 
next to the actual output to compare the answers.

All tests performed in the assignment check the bound cases and a few cases in the middle in order to insure the proper answers.


Question 4

(i)
I was not able to get the let expression working for the metacircular interpreter

(ii)
;To switch from lexical to dynamic scoping, we have to add an environment parameter 
;to the apply procedure and have the call to apply in eval be passed the environment as a third parameter.
;We need to do this because in dynamic scoping when we call a procedure we are extending the 
;environment that we are in at the time of the call whereas in lexical scoping the environment where
;the procedure was defined is the one that is extended.