/* (method, received, lower incl, upper excl) */
exception IndexOutOfBounds(string, int, int, int);

/* (method, index received) */
exception NegativeIndex(string, int);

/* (method) */
exception Empty(string);

/* (method) */
exception UnexpectedNone(string);

/* (method) */
exception Imaginary(string);

/* (method) */
exception Undefined(string);

/* (method) */
exception InternalError(string);

/* (method, needed number of elements) */
exception NotEnoughElements(string, int);

/* (method, explanation) */
exception InvalidArguments(string, string);

/* (method, explanation) */
exception EmptyRange(string, string);
