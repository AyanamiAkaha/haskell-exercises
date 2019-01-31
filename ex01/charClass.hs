import Data.Char
import Data.List

mapToCategory :: [Char] -> [(GeneralCategory, Char)]
mapToCategory xs = map (\c -> (generalCategory c, c)) xs

getCategories :: [Char] -> [GeneralCategory]
getCategories xs = nub . map generalCategory $ xs

groupToCategories :: [Char] -> [(GeneralCategory, [Char])]
groupToCategories xs = 
  map (\cat -> (cat, filterCat cat xs)) (getCategories xs)
  where
    filterCat :: GeneralCategory -> [Char] -> [Char]
    filterCat cat = filter (\c -> generalCategory c == cat)

