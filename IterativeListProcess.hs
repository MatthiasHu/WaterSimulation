module IterativeListProcess where

processIteratively :: ((a, b) -> (a, c)) -> a -> [b] -> (a, [c])
processIteratively _ a [] = (a, [])
processIteratively f a (b:bs) = (a'', b':bs')
                              where (a', b') = f (a, b)
                                    (a'', bs') = processIteratively f a' bs
