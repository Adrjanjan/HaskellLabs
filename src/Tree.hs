module Tree where

data Tree a = Leaf
            | Branch
            { value::a
            , left::Tree a
            , right::Tree a
            } deriving(Show, Eq)

t1 = Branch 0 Leaf Leaf
t2 = Branch 0 (Branch (-1) Leaf Leaf) Leaf
t3 = Branch 8 (Branch 4  (Branch 2 Leaf Leaf)
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
-------------------------------------------
-- | Check Tree emptiness
isEmpty :: Tree a -> Bool
isEmpty Branch {} = False
isEmpty Leaf = True
-------------------------------------------
-- | Check if Tree is binary
isBinary :: (Eq a, Ord a) => Tree a -> Bool
isBinary Leaf = True
isBinary (Branch v l r) = (l == Leaf || value l < v) && (r == Leaf || value r >= v) && isBinary l && isBinary r
-------------------------------------------
-- | Check if element is in a Tree
search:: (Eq a, Ord a) => Tree a -> a -> Bool
search Leaf _ = False
search (Branch v l r) to_find | to_find == v = True
                              | to_find <  v = search l to_find
                              | to_find >  v = search r to_find

-------------------------------------------
-- | Find Tree height
height :: Tree a -> Int
height Leaf = 0
height (Branch _ l r) = 1 + max (height l) (height r)

-------------------------------------------
-- | Check if Tree is balanced
isBalanced:: Tree a -> Bool
isBalanced (Branch _ l r)
  | abs (height l - height r) <= 1 = True
  | otherwise = False

-------------------------------------------
-- | Parse Tree to String
toString :: Show a => Tree a -> String
toString Leaf = ""
toString (Branch v l r) = show v ++ " (" ++ toString l ++ ", " ++ toString r ++ ")"

-------------------------------------------
-- | Return number of Branches
nnodes :: Tree a -> Int
nnodes Leaf = 0;
nnodes (Branch _ l r) = 1 + nnodes l + nnodes r

-------------------------------------------
-- | Sum values in Branches
nsum :: Num a => Tree a -> a
nsum Leaf = 0;
nsum (Branch v l r) = v + nsum l + nsum r

-------------------------------------------
-- | map equivalent for Tree
tmap :: (a -> b) -> Tree a -> Tree b
tmap func Leaf  = Leaf
tmap func (Branch v l r) = Branch (func v) (tmap func l) (tmap func r)

-------------------------------------------
-- | Find min in a Tree
minVal :: (Eq a, Ord a) => Tree a -> a
minVal (Branch v Leaf _) = v
minVal (Branch _ l _) = minVal l

-------------------------------------------
-- | Remove element from a Tree
remove :: (Eq a, Ord a) => Tree a -> a -> Tree a
remove (Branch v l r) to_remove | to_remove < v = Branch v (remove l to_remove) r
                                | to_remove > v = Branch v l (remove r to_remove)

remove (Branch _ Leaf Leaf)  _ = Leaf
remove (Branch _ l Leaf) _ = l
remove (Branch _ Leaf r) _ = r

remove (Branch _ l r) _ = Branch min l r'
  where
    min = minVal r
    r' = remove r min

-------------------------------------------
-- | Merge two Trees
merge :: (Ord a) => Tree a -> Tree a -> Tree a
merge tree Leaf = tree
merge Leaf tree = tree
merge tree (Branch v l r) = insert merged2 v
  where merged1 = merge tree l
        merged2 = merge merged1 r

