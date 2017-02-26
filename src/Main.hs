module Main where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity(..))
import Control.Monad (forever, join)
import System.CPUTime (getCPUTime)

main :: IO ()
main = do
  putStrLn "hello world"

data Behavior next a = AndThen {
  now :: a,
  future :: next (Behavior next a)
}

data Event next a
  = Occurred a
  | Later (next (Event next a))

always :: Applicative next => a -> Behavior next a
always a = a `AndThen` pure (always a)

never :: Applicative next => Event next a
never = Later (pure never)

sample :: Applicative next => Event next a -> Behavior next b -> Event next b
sample (Occurred a) behavior = Occurred (now behavior)
sample (Later nextEvent) behavior = Later (liftA2 sample nextEvent (future behavior))

plan :: Functor next => Event next (next a) -> next (Event next a)
plan (Occurred nextA) = fmap Occurred nextA
plan (Later nextEventA) = fmap (Later . plan) nextEventA

poll :: Applicative next => Behavior next (next a) -> next (Behavior next a)
poll behavior = liftA2 AndThen (now behavior) (fmap poll (future behavior))

instance Functor next => Functor (Event next) where
  fmap f (Occurred a) = Occurred (f a)
  fmap f (Later nextEvent) = Later (fmap (fmap f) nextEvent)

instance Functor next => Functor (Behavior next) where
  fmap f behavior = f (now behavior) `AndThen` fmap (fmap f) (future behavior)

instance Applicative next => Applicative (Behavior next) where
  pure = always
  bf <*> ba = (now bf) (now ba) `AndThen` liftA2 (<*>) (future bf) (future ba)

instance Show a => Show (Behavior next a) where
  show behavior = show $ now behavior

runForever :: Monad next => (a -> next b) -> next (Behavior next a) -> next c
runForever whatToDo nextBehavior = do
  behavior <- nextBehavior
  whatToDo (now behavior)
  runForever whatToDo (future behavior)

type Next = IO

getTime :: Next String
getTime = pure "time"

getName :: Next String
getName = getLine

time :: Next (Behavior Next String)
time = poll (always getTime)

name :: Next (Behavior Next String)
name = poll (always getName)

both :: Next (Behavior Next String)
both = do
  t <- poll (always getCPUTime)
  n <- name
  return $ (\t n -> t ++ " - " ++ n) <$> (fmap show t) <*> n