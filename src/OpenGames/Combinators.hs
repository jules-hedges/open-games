{-# LANGUAGE GADTs #-}

module OpenGames.Combinators (
    OG (..), play, coplay, equilibrium
  ) where

data OG a x y r s where
  Decision :: (x -> (y -> r) -> y -> Bool) -> OG (x -> y) x y r ()
  Arr      :: (x -> y) -> (r -> s) -> OG () x y r s
  (:***:)  :: OG a x y r s -> OG b x' y' r' s' -> OG (a, b) (x, x') (y, y') (r, r') (s, s')
  (:>>>:)  :: OG a x y s t -> OG b y z r s -> OG (a, b) x z r t
  Counit   :: OG () x () () x
  Reindex  :: (a -> b) -> OG b x y r s -> OG a x y r s

play :: OG a x y r s -> a -> x -> y
play (Decision _) a x           = a x
play (Arr f _) () x             = f x
play (g :***: h) (a, b) (x, x') = (play g a x, play h b x')
play (g :>>>: h) (a, b) x       = play h b (play g a x)
play Counit () _                = ()
play (Reindex f g) a x          = play g (f a) x

coplay :: OG a x y r s -> a -> x -> r -> s
coplay (Decision _) _ _ _                 = ()
coplay (Arr _ f) () _ r                   = f r
coplay (g :***: h) (a, b) (x, x') (r, r') = (coplay g a x r, coplay h b x' r')
coplay (g :>>>: h) (a, b) x r             = coplay g a x (coplay h b (play g a x) r)
coplay Counit () x ()                     = x
coplay (Reindex f g) a x r                = coplay g (f a) x r

equilibrium :: OG a x y r s -> x -> (y -> r) -> a -> Bool
equilibrium (Decision e) x k a           = e x k (a x)
equilibrium (Arr _ _) _ _ _              = True
equilibrium (g :***: h) (x, x') k (a, b) = equilibrium g x (\y -> fst $ k (y, play h b x')) a
                                        && equilibrium h x' (\y' -> snd $ k (play g a x, y')) b
equilibrium (g :>>>: h) x k (a, b)       = equilibrium g x (\y -> coplay h b y $ k $ play h b y) a
                                        && equilibrium h (play g a x) k b
equilibrium Counit _ _ _                 = True
equilibrium (Reindex f g) x k a          = equilibrium g x k (f a)
