import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Tuple    as T 

data MTree a = MTree {rootLabel :: a, subForest :: MForest a} 
               deriving (Eq, Ord)

type MForest a = [MTree a]

mTreeIndent :: Int
mTreeIndent = 4

mTreeBranchChar :: Char
mTreeBranchChar = '.'

mTreeNodeChar :: Char
mTreeNodeChar = '+'

instance (Show a) => Show (MTree a) where
  show = go 0
    where
      go nTabs MTree {rootLabel = rl, subForest = mts} =
          L.replicate nTabs mTreeBranchChar ++
          (if nTabs > 0 then " " else "")   ++
          mTreeNodeChar:" root label="      ++ 
          show rl                           ++
          "\n"                              ++
          F.foldr f "" mts
        where
          f mt acc = go (nTabs + mTreeIndent) mt ++ acc

mTreeMk :: a -> [MTree a] -> MTree a
mTreeMk rl mts = MTree {rootLabel = rl, subForest = mts}

mTreeMkLeaf :: a -> MTree a
mTreeMkLeaf rl = mTreeMk rl []

mTreeExample :: MTree Integer 
mTreeExample = mTreeMk 6 [t1, t2, t3] where
  t1 = mTreeMk 4 [t4,t5]
  t2 = (mTreeMk 2 [(mTreeMk 3 [mTreeMkLeaf 2])])
  t3 = (mTreeMk 8 [t6,(mTreeMkLeaf 9),t7])
  t4 = (mTreeMk 2 [(mTreeMkLeaf 1),(mTreeMkLeaf 7),(mTreeMkLeaf 3)])
  t5 = (mTreeMk 5 [(mTreeMkLeaf 3),(mTreeMkLeaf 9)])
  t6 = (mTreeMk 7 [(mTreeMkLeaf 8)])
  t7 = (mTreeMk 2 [(mTreeMkLeaf 1),(mTreeMkLeaf 2),(mTreeMkLeaf 9),(mTreeMkLeaf 7),(mTreeMkLeaf 3)])



mTreeCount :: Num b => MTree a -> b -- qui calcule le nombre de nœuds d’un arbre général.
mTreeCount (MTree _ []) = 1
mTreeCount (MTree _ ts) = 1 + sum (map mTreeCount ts)

mTreeIsLeaf :: MTree a -> Bool -- qui décide si un arbre général est une feuille. 
mTreeIsLeaf (MTree _ []) = True
mTreeIsLeaf _ = False

mTreeLeaves :: MTree a -> [a] -- qui retourne la liste des feuilles d’un arbre général.
mTreeLeaves (MTree x []) = [x]
mTreeLeaves (MTree _ ts) = concatMap mTreeLeaves ts

mTreeCountLeaves :: Num b => MTree a -> b --qui calcule le nombre de feuilles d’un arbre général.
mTreeCountLeaves (MTree _ []) = 1
mTreeCountLeaves (MTree _ ts) = sum (map mTreeCountLeaves ts)

mTreeSum :: Num a => MTree a -> a -- qui calcule la somme des étiquettes d’un arbre général.
mTreeSum (MTree x ts) = x + sum (map mTreeSum ts)

mTreeToList :: MTree a -> [a] -- qui retourne la liste des étiquettes d’un arbre général. 
mTreeToList (MTree x []) = [x]
mTreeToList (MTree x ts) = x : concatMap mTreeToList ts

mTreeHeight :: (Num b, Ord b) => MTree a -> b -- qui calcule la hauteur d’un arbre général.
mTreeHeight (MTree x []) = 1
mTreeHeight (MTree x ts) = 1 + maximum (map mTreeHeight ts)

mTreeElem :: Eq a => a -> MTree a -> Bool -- qui décide si une étiquette donnée appraît dans un arbre général.
mTreeElem e (MTree x ts) = x == e || any (mTreeElem e) ts

mTreeMin :: Ord a => MTree a -> a -- qui calculent l’étiquette minimale
mTreeMin ts = minimum (mTreeToList ts)

mTreeMax :: Ord a => MTree a -> a -- qui calculent l’étiquette maximale d’un arbre général
mTreeMax ts = maximum (mTreeToList ts)


-- Exo 2

mTreeDepthFirstTraversal :: MTree a -> [a]
mTreeDepthFirstTraversal (MTree x []) = [x]
mTreeDepthFirstTraversal (MTree x ts) = x : concatMap mTreeDepthFirstTraversal ts

mTreeBreadthFirstTraversal :: MTree a -> [a]
mTreeBreadthFirstTraversal t = go [t]
  where
    go [] = []
    go nts = map rootLabel nts ++ go (concatMap subForest nts)

mTreeLayer :: Int -> MTree a -> [a]
mTreeLayer n t = go n [t]
  where
    go _ [] = []
    go 0 _ = []
    go 1 nts = map rootLabel nts
    go n nts = go (n - 1) (concatMap subForest nts)


mTreeLayerToList :: MTree a -> [[a]]
mTreeLayerToList t = layer [] [t]
  where
    layer acc [] = reverse acc
    layer acc nodes = layer (map rootLabel nodes : acc) (concatMap subForest nodes)



-- Exo 3

mTreeMap :: (a -> b) -> MTree a -> MTree b
mTreeMap f (MTree x ts) = MTree (f x) (map (mTreeMap f) ts)

mTreeFilter :: (a -> Bool) -> MTree a -> MForest a
mTreeFilter p (MTree x ts)
  | p x = [MTree x (concatMap (mTreeFilter p) ts)]
  | otherwise = concatMap (mTreeFilter p) ts

mTreeFold1 :: (a -> b -> b) -> b -> MTree a -> [b]
mTreeFold1 f z (MTree r []) = [f r z] 
mTreeFold1 f z (MTree r l) = [f r y | mt <- l, y <- mTreeFold1 f z mt]

mTreeFold2 :: (a -> [b] -> b) -> MTree a -> b 
mTreeFold2 f mt = f rl xs
  where
    rl = rootLabel mt
    xs = L.map (mTreeFold2 f) mts
      where
        mts = subForest mt

mTreeCollectPaths :: MTree a -> [[a]]
mTreeCollectPaths = mTreeFold1 (:) []

mTreeSignature :: (Num a) => MTree a -> [a] 
mTreeSignature = mTreeFold1 (+) 0

{- mTreeMin :: Ord a => MTree a -> a
mTreeMin t = mTreeFold2 (\x acc -> if x < acc then acc = x else acc) (rootLabel t) t
 -}