-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
-- Copyright (c) 1998 Cambridge University Press
-- Documentacion hecha por Andreth Salazar y Vicente Santacoloma USB VE

module LeftistHeap (module Heap, LeftistHeap(..)) where
  import Heap

  data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving Show
  
  rank:: LeftistHeap a -> Int
  rank E = 0
  rank (T r _ _ _) = r

  makeT:: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
  makeT x a b = if rank a >= rank b then T (rank b + 1) x a b
                else T (rank a + 1) x b a
                
  instance Heap LeftistHeap where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x h = merge (T 1 x E E) h

    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
      if x <= y then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge h1 b2)

    findMin E = error "LeftistHeap.findMin: empty heap"
    findMin (T _ x a b) = x

    deleteMin E = error "LeftistHeap.deleteMin: empty heap"
    deleteMin (T _ x a b) = merge a b
    
      
    
    
  
  

   
    
    
    