module Greek
where

data Greek = Greek [Letter]
           deriving (Eq, Show)

data Letter = Letter { base :: Char,
                       iota-sub :: Boolean,
                       breathing :: Breathing,
                       accent :: Accent,
                       macron :: Boolean }
            deriving (Eq, Show)

data Breathing = NoBreath | Smooth | Rough
               deriving (Eq, Ord, Enum, Show)

data Accent = NoAccent | Acute | Grave | Circumflex
            deriving (Eq, Ord, Enum, Show)

refineCompare :: [Ordering] -> Ordering
refineCompare [] = EQ
refineCompare (GT:_) = GT
refineCompare (EQ:orders) = refineCompare orders
refineCompare (LT:_) = LT

listCompare :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
listCompare _ [] [] = EQ
listCompare _ _ [] = LT
listCompare _ [] _ = GT
listCompare compare (x:xs) (y:ys) =
  case compare x y of
    GT -> GT
    EQ -> listCompare compare xs ys
    LT -> LT

lowercaseBaseCompare :: Char -> Char -> Ordering
lowercaseBaseCompare 'ϝ' c
  | c <= 'ε' = GT
  | c == 'ϝ' = EQ
  | otherwise = LT
lowercaseBaseCompare c 'ϝ'
  | c <= 'ε' = LT
  | c == 'ϝ' = EQ
  | otherwise = GT
lowercaseBaseCompare 'ς' c
  | c <= 'ρ' = GT
  | c == 'ς' = EQ
  | otherwise = LT
lowercaseBaseCompare c 'ς'
  | c <= 'ρ' = LT
  | c == 'ς' = EQ
  | otherwise = LT
lowercaseBaseCompare c1 c2 = compare c1 c2

baseCompareCI :: Char -> Char -> Ordering
