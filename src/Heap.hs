-- Source code from
--   Purely Functional Data Structures
--   Chris Okasaki
--   Cambridge University Press, 1998
-- Copyright (c) 1998 Cambridge University Press

-- | Modulo general que representa el modelo de firmas 
-- | del heap como conjunto ordenado.
module Heap (Heap(..)) where
  class Heap h where
    empty     :: Ord a => h a
    isEmpty   :: Ord a => h a -> Bool

    insert    :: Ord a => a -> h a -> h a
    merge     :: Ord a => h a -> h a -> h a

    findMin   :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a
