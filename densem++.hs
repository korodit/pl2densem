-- Ορέστης Ροδίτης-Κουτσαντώνης Α.Μ. 03111052
-- Ο διερμηνέας δέχεται έγκυρο πρόγραμμα WHILE++ από το standard input,
-- και επιστρέφει την τελική τιμή της μεταβλητής με το όνομα "result"
import ReadDensem

semC :: C -> S -> S
semC Cskip s = s
semC (Cexpr x) s = b
  where (a,b) = (semN x s)
semC (Cseq c1 c2) s = semC c2 (semC c1 s)
semC (Cfor n c) s = expon i (semC c) s
  where (i,_) = semN n s
semC (Cif b c1 c2) s | semB b s  = semC c1 s
                     | otherwise = semC c2 s
semC (Cwhile b c) s = fix bigF s
  where bigF f s | semB b s  = f (semC c s)
                 | otherwise = s

semN :: N -> S -> (Integer,S)
semN Nzero s = (0,s)
semN (Nsucc n) s = (x+1,y)
  where (x,y) = (semN n s)
semN (Npred n) s = (x-1,y)
  where (x,y) = (semN n s)
semN (Nvar x) s = (s x,s)
semN (Nassign x n) s = (val,update s x val)
  where (val,news) = (semN n s)
semN (Ninc x) s = semN (Nassign x (Nsucc (Nvar x))) s
  -- where (inced,_) = semN (Nsucc (Νvar x)) s
semN (Ndec x) s = semN (Nassign x (Npred (Nvar x))) s
--   where (deced,_) = semN (Npred (Nvar x)) s

semB :: B -> S -> Bool
semB Btrue s = True
semB Bfalse s = False
semB (Blt n1 n2) s = x1 < x2
  where 
    (x1,y1) = semN n1 s
    (x2,y2) = semN n2 s
semB (Beq n1 n2) s = x1 == x2
  where
    (x1,y1) = semN n1 s
    (x2,y2) = semN n2 s
semB (Bnot b) s = not (semB b s)

-- auxiliary functions

fix f = f (fix f)

expon 0 f = id
expon n f = f . expon (n-1) f

update s x n y | x == y    = n
               | otherwise = s y

s0 x = error ("not initialized variable " ++ x)

run c = print ((semC c s0) "result")

makeN 0 = Nzero
makeN n = Nsucc (makeN (n-1))

ex1 = Cseq (Cexpr (Nassign "result" Nzero))
           (Cfor (makeN 6) (
              Cfor (makeN 7) (
                Cexpr (Nassign "result" (Nsucc (Nvar "result")))
              )
           ))

ex2 = Cseq (Cexpr (Nassign "x" (makeN 42)))
      (Cseq (Cexpr (Nassign "result" Nzero))
            (Cwhile (Blt Nzero (Nvar "x"))
              (Cseq (Cexpr (Nassign "x" (Npred (Nvar "x"))))
                    (Cexpr (Nassign "result" (Nsucc (Nvar "result")))))))

-- ex2 = Cexpr (Nassign "result" (Nsucc (Nsucc (Nsucc Nzero))))
ex3 = Cseq (Cexpr (Nassign "result" (makeN 5))) (Cexpr (Ndec "result"))

ex4 = Cseq (Cseq (Cexpr (Nassign "result" Nzero))
           (Cfor (makeN 6) (
              Cfor (makeN 7) (
                Cexpr (Ninc "result" )
              )
           )))

           (Cexpr (Ndec "result"))

main = do  
  input <- getContents
  let c :: C
      c = read input
      -- c = ex4
  -- print c
  -- print c
  run c
  -- print (run ex2)