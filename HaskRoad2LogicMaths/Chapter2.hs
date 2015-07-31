{-# LANGUAGE FlexibleInstances #-}
module Code.Pratice.HaskRoad2LogicMaths.Chapter2 where

  infix 1 ==>
  (==>) :: Bool -> Bool -> Bool
  p ==> q = (not p) || q

  infix 1 <=>
  (<=>) :: Bool -> Bool -> Bool
  x <=> y = x == y

  -- Exercise 2.2
  (\/) :: Bool -> Bool -> Bool
  True \/ p = not p
  False \/ p = p

  -- Exercise 2.4
  infixr 2 <+>

  (<+>) :: Bool -> Bool -> Bool
  x <+> y = x /= y

  class TF p where
    valid  :: p -> Bool
    lequiv :: p -> p -> Bool

  instance TF Bool
    where
      valid      = id
      lequiv p q = p == q

  instance TF p => TF (Bool -> p)
    where
      valid  f   = and [valid (f q) | q <- [True, False]]
      lequiv f g = and [lequiv (f q) (g q) | q <- [True, False]]

  -- Exercise 2.9
  formula1 p q = (p <+> q) <+> q
  formula2 p q = p
  exercise2_9 = lequiv formula1 formula2

  -- law of double negation
  law1 = lequiv id (\p -> not $ not p)
  -- law of idempotence
  law2 = law21 && law22
         where law21 = lequiv id (\p -> p && p)
               law22 = lequiv id (\p -> p || p)
  law3 = law31 && law32
         where law31 = lequiv (\p q ->       p ==> q) (\p q -> not p ||     q)
               law32 = lequiv (\p q -> not $ p ==> q) (\p q ->     p && not q)
  -- law of contraposition
  law4 = and [law41, law42, law43]
         where law41 = lequiv (\p q -> not p ==> not q) (\p q ->     q ==>     p)
               law42 = lequiv (\p q ->     p ==> not q) (\p q ->     q ==> not p)
               law43 = lequiv (\p q -> not p ==>     q) (\p q -> not q ==>     p)
  law5 = law51 && law52
         where law51 = lequiv (\p q -> p <=> q) (\p q -> (p ==> q) && (q ==> p))
               law52 = lequiv (\p q -> p <=> q) (\p q -> (p && q) || (not p && not q))
  -- law of commutativity
  law6 = law61 && law62
         where law61 = lequiv (&&) $ flip (&&)
               law62 = lequiv (||) $ flip (||)
  -- DeMorgan laws
  law7 = law71 && law72
         where law71 = lequiv (\p q -> not (p && q)) (\p q -> not p || not q)
               law72 = lequiv (\p q -> not (p || q)) (\p q -> not p && not q)
  -- Laws of associativity
  law8 = law81 && law82
         where law81 = lequiv (\p q r -> p && q && r) (\p q r -> p && (q && r))
               law82 = lequiv (\p q r -> p || q || r) (\p q r -> p || (q || r))
  law9 = law91 && law92
         where law91 = lequiv (\p q r -> p && (q || r)) (\p q r -> (p && q) || (p && r))
               law92 = lequiv (\p q r -> p || (q && r)) (\p q r -> (p || q) && (p || r))
  law10 = lequiv (\p -> p ==> False) not

  -- dominance laws
  law11 = law111 && law112
          where law111 = lequiv ((||) True) (\p -> True)
                law112 = lequiv ((&&) False) (\p -> False)

  -- identify laws
  law12 = law121 && law122
          where law121 = lequiv ((||) False) id
                law122 = lequiv ((&&) True) id

  -- law of excluded middle
  law13 = lequiv (\p -> not p || p) (\p -> True)

  -- contradiction
  law14 = lequiv (\p -> not p && p) (\p -> False)
