{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State rs) = snd . rs
  -- error "todo: Course.State#exec"

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State rs) = fst . rs
  -- error "todo: Course.State#eval"

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\v -> (v, v))
  -- error "todo: Course.State#get"

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put s = State (\_ -> ((), s))
  -- error "todo: Course.State#put"

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  (<$>) f (State rs) = State (mapFst . rs)
    where mapFst (a, b) = (f a, b)

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a, s))
    -- error "todo: Course.State pure#instance (State s)"
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) (State rs) (State rs') = State (mapFst)
    where mapFst s = do
            let (f, ns) = rs s
            let (a, fs) = rs' ns
            (f a, fs)

  -- run rs s, get the f and the ns, run rs' on ns to get the (a, fs), transform to tuple of (f a, fs)
  -- rs :: s -> ((a -> b), s)
  -- rs':: s -> (a, s)
  ----
  -- ?? :: \s -> (b, s)
    -- error "todo: Course.State (<*>)#instance (State s)"

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  (=<<) f (State rs) = State (\s -> do
                                 let (a, ns) = rs s
                                 let q = f a
                                 runState q ns)

  -- rs :: s -> (a, s)
  -- f  :: a -> State s b
  ----
  -- inner func :: s -> (b, s)
  -- State s b
    -- error "todo: Course.State (=<<)#instance (State s)"

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM f items = foldRight check (pure Empty) items
  where check a foa = f a >>= \result -> if result then pure (Full a) else foa
  -- pure $ find (\a -> _ $ (pure a) >>= f) items
  -- check :: a -> f (Optional a) -> f (Optional a)
  -- error "todo: Course.State#findM"

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
-- firstRepeat = error "todo"--findM addAndCheck
    -- where addAndCheck item = error "meow"
-- firstRepeat as = eval $ findM (addAndCheck startingState) as
--     where addAndCheck :: State s Bool -> a -> State s Bool
--           addAndCheck currentState item = undefined
--           tracker a = State (\s -> case a `member` s of
--                                       True -> (True, insert a s)
--                                       False -> ())
firstRepeat xs =
  eval (findM f xs) S.empty
  where f a = State (\s -> if S.member a s then (True, s) else (False, S.insert a s))

-- State stateType valueType
-- runState :: s -> (a, s)
-- runSTate :: stateType -> (valueType, stateType+1)

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
-- distinct xs = toList $ eval (filtering f xs) S.empty
--   where f a = State (\s -> if S.member a s then ((), s) else ((), S.insert a s))
distinct xs = eval (filtering f xs) S.empty
  where f a = (\s -> S.notMember a s <$ put (S.insert a s )) =<< get
  -- error "todo: Course.State#distinct"
  -- filtering :: Applicative f => (a -> f Bool) -> List a -> f (List a)

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy n = findOne n == Full 1
  where sumOfSquare m = toInteger $ sum $ square <$> digitToInt <$> show' m
        square = join (*)
        pump = produce sumOfSquare
        findOne m = firstRepeat (pump m)

        -- firstRepeat (produce (toInteger . sum . (square <$>) . digits) n) == Full 1
  -- error "todo: Course.State#isHappy"
-- produce :: (a -> a) -> a -> List a
-- firstRepeat :: Ord a => List a -> Optional a
