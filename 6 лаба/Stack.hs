
module Stack(Stack,push,pop,top,emptyStack,stackEmpty) where
import Prelude
newtype Stack a  = Stk [a]
 
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)
 
pop :: Stack a -> Stack a 
pop (Stk []) = error "pop from an empty stack"
pop (Stk (_:xs)) = Stk xs
 
top :: Stack a -> a 
top (Stk []) = error "top from an empty stack"
top (Stk (x:_)) = x 
 
emptyStack :: Stack a
emptyStack = Stk [] 
 
stackEmpty :: Stack a -> Bool
stackEmpty (Stk []) = True
stackEmpty (Stk _) = False

