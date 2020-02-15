# Target operations

A small set of supported operations:

```python
ordering_of_classes = [
    # singleton numbers
    'Zero', 'One', 'NegativeOne',  # Number, Singleton
    # numbers
    'Integer', # Rational
    'Rational', # Number
    'Float', # Number
    # singleton symbols
    'Exp1', 'Pi', # Number, Singleton, NumberSymbol
    # symbols
    'Symbol', # 
    # arithmetic operations
    'Pow', 'Mul', 'Add', # Expr, AssocOp
    # defined singleton functions
    'Sqrt',
    'Exp', 'Log',
    'Sin', 'Cos',  # Function
]
```