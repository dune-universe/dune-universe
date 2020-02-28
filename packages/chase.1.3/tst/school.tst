% chase version 1.3
% bound = 250, limit = 2000
% ********
% teacher(T) => person(T). % (0)
% class(C) => T = instructor_of(C). % (1)
% instructor_of(C) = T => teacher(T). % (2)
% class(math). % (3)
% class(english). % (4)
% instructor_of(math) = instructor_of(english). % (5)
% ********

(6,5){5}![teacher(t), person(t), class(math), class(english),
  instructor_of(math) = t, instructor_of(english) = t, math = math,
  english = english]
