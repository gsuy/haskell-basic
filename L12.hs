data Tree a = Empty
    | Node (Tree a) a (Tree a)
  deriving (Show,Eq,Ord)

preorder Empty         = []
preorder (Node l x r)  =
    [x] ++ preorder l ++ preorder r
inorder Empty          = []
inorder (Node l x r)   =
    inorder l ++ [x] ++ inorder r
postorder Empty        = []
postorder (Node l x r) =
    postorder l ++ postorder r ++ [x]

map_t _ Empty = Empty
map_t f (Node l x r) = Node (map_t f l) (f x) (map_t f r)

foldl_preorder _ s Empty = s
foldl_preorder f s tree = foldl f s (preorder tree)
foldr_preorder _ s Empty = s
foldr_preorder f s tree = foldr f s (preorder tree)

foldl_inorder _ s Empty = s
foldl_inorder f s tree = foldl f s (inorder tree)
foldr_inorder _ s Empty = s
foldr_inorder f s tree = foldr f s (inorder tree)

foldl_postorder _ s Empty = s
foldl_postorder f s tree = foldl f s (postorder tree)
foldr_postorder _ s Empty = s
foldr_postorder f s tree = foldr f s (postorder tree)

height Empty = 0
height (Node l _ r) = if height l > height r then (height l) + 1 else (height r) + 1

getData node = case node of
    Node _ a _ -> a

isBST Empty = True
isBST (Node l x r)
    | ((l/=Empty) && (getData l >= x)) || ((r/=Empty) && (getData r < x)) = False
    | otherwise = isBST(l) && isBST(r)


--Node (Node Empty 4 Empty) 9 (Node Empty 7 Empty)
--(Node (Node Empty 4 Empty) 9 (Node Empty 10 (Node Empty 11 Empty)))

