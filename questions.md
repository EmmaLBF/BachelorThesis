UpdateVar result is of type CExpression (), this causes errors when I try to use in it a sequence since the return value type is () and not a. The only solution I have is 'returning' a variable afterwards.

How should I translate recursion? I tried to make a function definintion which references itself but it was giving inifite type errors. While loop method isn't working very well either, functions for fac, difficult for fib.

The eval function does not make sense without the expression/statement split
I cannot return any value for UpdateVar or DefVar, a while loop does not return anything but I need to make it. I think I still need the split but that brings me back to partial translation and eval functions.
