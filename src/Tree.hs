module Tree where

data Tree a = Leaf
            | Branch
            { value::a
            , left::Tree a
            , right::Tree a
            } deriving(Show, Eq)

--data Tree a = Leaf | Branch a (Tree a) (Tree a)

drzewo = Branch 0 Leaf Leaf
drzewo1 = Branch 0 (Branch (-1) Leaf Leaf) Leaf
drzewo2 = Branch 8 (Branch 4  (Branch 2 Leaf Leaf)
                              (Branch 7 Leaf Leaf))
                   (Branch 11 (Branch 9 Leaf Leaf)
                              (Branch 13 Leaf Leaf))

-------------------------------------------
-- | Tree traversing functions
vlr :: Tree a -> [a]
lvr :: Tree a -> [a]
lrv :: Tree a -> [a]
vrl :: Tree a -> [a]
rvl :: Tree a -> [a]
rlv :: Tree a -> [a]

vlr Leaf = []
vlr (Branch v l r) = [v] ++ vlr l ++ vlr r

lvr Leaf = []
lvr (Branch v l r) = lvr l ++ [v] ++ lvr r

lrv Leaf = []
lrv (Branch v l r) = lrv l ++ lrv r ++ [v]

vrl Leaf = []
vrl (Branch v l r) = [v] ++ vrl r ++ vrl l

rvl Leaf = []
rvl (Branch v l r) = rvl r ++ [v] ++ rvl l

rlv Leaf = []
rlv (Branch v l r) = rlv r ++ rlv l ++ [v]
-------------------------------------------
-- | Insert new value into tree
insert :: Ord a => Tree a -> a -> Tree a
insert Leaf to_put = Branch to_put Leaf Leaf
insert (Branch v l r) to_put | to_put < v   = Branch v (insert l to_put) r
                             | to_put >= v  = Branch v l (insert r to_put)


