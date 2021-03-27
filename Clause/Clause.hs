module Clause where

import qualified Data.Set as Set

type Cnf = Set.Set Clause
newtype Clause = Clause (Set.Set Int) deriving (Eq)

instance Ord Clause where
    compare (Clause c) (Clause d) = case compare (Set.size c) (Set.size d) of
        EQ -> compare c d
        ord -> ord

instance Show Clause where
    show (Clause c) = show c
