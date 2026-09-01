module Automata (drawAutomata) where

import Codec.Picture (Image, PixelRGB8 (..), generateImage, writePng)
import Data.Array (listArray, (!))
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf (printf)

intToBin :: Int -> String
intToBin = printf "%08b"

binToInt :: String -> Int
binToInt =
  foldl' (\accum dig -> accum * 2 + digitToInt dig) 0

myelems :: [String]
myelems = ["0", "1"]

intToClr :: Char -> Char
intToClr x = if x == '0' then 'w' else 'b'

myClrLst :: [String]
myClrLst = map (map intToClr) [x ++ y ++ z | x <- myelems, y <- myelems, z <- myelems]

newtype Rule = Rule (Map String Char) deriving (Show)

makeRule :: Int -> Rule
makeRule myrulenum =
  let mcl = myClrLst
      blst = reverse $ intToBin myrulenum
   in Rule $ M.fromList $ zip mcl $ map intToClr blst

type Row = [Char]

nextRow :: Rule -> Row -> Row
nextRow rl rw =
  map (extractMapFromRule rl M.!) colPatts
  where
    extendedRow = [last rw] ++ rw ++ [head rw]
    colPatts = map triplet2List (zip3 extendedRow (drop 1 extendedRow) (drop 2 extendedRow))
    triplet2List (x, y, z) = [x, y, z]
    extractMapFromRule (Rule rm) = rm

renderPixel :: Char -> PixelRGB8
renderPixel 'b' = PixelRGB8 0 0 0
renderPixel 'w' = PixelRGB8 255 255 255
renderPixel _ = PixelRGB8 255 0 0 -- Red (Error case)

mkGridWithRule :: Int -> Int -> Int -> [String]
mkGridWithRule ruleNum width height =
  let halfRow = width `div` 2
      firstRow = replicate halfRow 'w' ++ ['b'] ++ replicate (halfRow - 1) 'w'
      rule = makeRule ruleNum
   in take height (iterate (nextRow rule) firstRow)

drawAutomata :: Int -> Int -> Int -> FilePath -> IO ()
drawAutomata ruleNum size scale path =
  do writePng path img
     putStr path
  where
    rawGrid = mkGridWithRule ruleNum size size
    bounds = ((0, 0), (size - 1, size - 1))
    gridArr = listArray bounds (concat rawGrid)
    imgWidth = size * scale
    imgHeight = size * scale
    pixelGen x y =
      let logicalX = x `div` scale
          logicalY = y `div` scale
          cellChar = gridArr ! (logicalY, logicalX)
       in renderPixel cellChar
    img = generateImage pixelGen imgWidth imgHeight

