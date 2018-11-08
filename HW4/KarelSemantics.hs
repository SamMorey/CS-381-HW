-- Made by:
-- Arya Asgari (932 125 581) - Asgaria
-- Sam Morey (932 448 079) - Moreys

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not tes) worl robo = not (test tes worl robo) 
test (Facing dir) worl robo = if (getFacing (robo) == dir) then True else False    
test (Clear dir) worl robo = isClear (relativePos dir robo) worl     
test (Beeper) worl robo = hasBeeper (getPos robo) worl   
test (Empty) worl robo = isEmpty robo          

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move _ w r = let p = relativePos Front r 
                  in if (isClear p w) 
                        then OK w (setPos p r) 
                        else Error  ("Blocked at: " ++ show p)

stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper _ w r = let p = getPos r
                       in if not (isEmpty r)
                             then OK (incBeeper p w) (decBag r)
                             else Error ("No beeper to put.")

stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)

stmt (Block []) _ w r = OK w r

stmt (Block (x:xs)) d w r = case stmt x d w r of
                          (OK ww rr) -> stmt (Block xs) d ww rr 
                          (Done rr) -> Done rr
                          (Error m) -> Error m

stmt (If t stm1 stm2) d w r = if (test t w r) then stmt stm1 d w r else stmt stm2 d w r

stmt (Call m) d w r = case lookup m d of
                       (Just a) -> stmt a d w r
                       _        -> Error ("Undefined macro: " ++ m)  

stmt (While t s) d w r = if test t w r then case stmt s d w r of
                                       (OK ww rr) -> stmt (While t s) d ww rr
                                       (Done rr) -> Done rr
                                       (Error m) -> Error m 
                         else OK w r

stmt (Iterate i s) d w r = if i > 0 then case stmt s d w r of
                                    (OK ww rr) -> stmt (Iterate (i-1) s) d ww rr
                                    (Done rr) -> Done rr
                                    (Error m) -> Error m
                           else OK w r

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
