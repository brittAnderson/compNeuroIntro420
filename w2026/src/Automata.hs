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

-- Now, at this point I need some guidance writing the function for actually drawing the png. Don't program it for me. Just give me some general pointers on structure and JuicyPixels functions I will want to use to take the output of mkGridWithRule and generate the png.

-- This is a great spot to be in. You have your logic (generation) and your rendering (pixel conversion) separated. Now you just need to bridge the gap between your data structure (`[String]`) and the expected format of the `JuicyPixels` library.

-- Here is how a Haskeller approaches this using `Codec.Picture`.

-- ### 1. The Core Function: `generateImage`

-- In imperative languages, you might iterate over an empty canvas and paint pixels. In Haskell, we define an image mathematically. We say: "For any coordinate $(x, y)$, here is the function to determine its color."

-- The function you are looking for is:
-- ```haskell
-- generateImage :: Pixel a => (Int -> Int -> a) -> Int -> Int -> Image a
-- ```

-- *   **Argument 1:** A generator function `f`. `f x y` must return a pixel.

-- *   **Argument 2:** The width of the image.

-- *   **Argument 3:** The height of the image.

-- ### 2. The Logic Bridge

-- Your grid comes out of `mkGridWithRule` as a `[String]` (which is effectively `[[Char]]`).
-- `generateImage` will ask you for the pixel at `x` (column) and `y` (row).

-- You need to figure out how to bridge these two concepts:
-- 1.  **Random Access:** `generateImage` supplies `x` and `y`.
-- 2.  **List Indexing:** Your grid is a linked list. To find the item at coordinate $(x, y)$, you effectively index the outer list (rows) by `y`, and the inner list (string) by `x`.

-- * Hint:* Haskell lists use the `!!` operator for indexing. So `list !! index` returns the element.

-- ### 3. Assembling the Pipeline

-- Your final function (let's call it `drawAutomata`) will likely look like this structurally:

-- 1.  **Generate Data:** Call `mkGridWithRule` to get your `[String]`.
-- 2.  **Define a Lookup:** Create a small `where` clause or lambda function that:
--     *   Takes an `x` and a `y`.
--     *   Finds the specific `Char` in your generated data.
--     *   Passes that `Char` to your existing `renderPixel` function.
-- 3.  **Create Image:** Pass that lookup function into `generateImage`.
-- 4.  **Save:** Use `writePng` (which performs the IO action) to save the resulting image to a file path.

-- ### 4. A Note on "The Superior Programmer" Approach

-- While using `!!` on lists is the easiest way to write this right now (and totally fine for a 256x256 image), a senior programmer would gently remind you that lists are **linked lists**.

-- Accessing an element at the end of a list is $O(n)$. Doing this for every pixel in an image makes the complexity $O(width \times height \times height)$.

-- *   **For now:** Stick with list indexing (`!!`) to get it working. It is idiomatic for small educational examples.

-- *   **For the future:** If you were generating 4k images, you would likely convert your `[String]` into a `Vector` (array) first to get $O(1)$ access times.

-- ### Summary of Functions to Look Up
-- 1.  `Codec.Picture.generateImage`
-- 2.  `Codec.Picture.writePng`
-- 3.  `Prelude.!!` (List indexing)

-- I don't want to have every mkGridWithRule be a pixel. I would like to specify a size (e.g. sqWidthHeight) to scale the image to have each x,y control a 10x10 (for example) grid size. Can we include that, and isn't there some way to just map the rows and columns rather than use the index approach?

-- You are hitting on a classic disconnect between **Lists** (sequential access) and **Images** (random access).

-- ### 1. Scaling: The Coordinate Approach
-- To make a 10x10 pixel block represent 1 unit of data, you don't necessarily need to change your data structure; you can change your lookup math.

-- When `generateImage` asks for the color at pixel `(45, 45)` and your scale is 10, you simply tell it to look at index `(4, 4)` in your data.

-- You use **Integer Division** (`div`).

-- $$ \text{index} = \lfloor \frac{\text{pixel}}{10} \rfloor $$

-- In Haskell:
-- ```haskell
-- -- Inside the function passed to generateImage
-- let gridX = pixelX `div` scale
-- let gridY = pixelY `div` scale
-- ```
-- This is the standard computer graphics way to handle semantic zooming.

-- ### 2. "Can't I just map?" (The Data Inflation Approach)
-- You asked if you could just map the rows and columns rather than using indices. You **can**, but it involves transforming your data before drawing it.

-- If you have `["wb"]` and you want a 2x scale, you need to turn it into:
-- ```haskell
-- [ "wwbb"
-- , "wwbb" ]
-- ```

-- You can achieve this using `concatMap` and `replicate`. This fits your desire to specificially use list mapping functions:

-- 1.  **Expand the columns:** Map over every `Char` in a row and replace it with $N$ copies of itself.
-- 2.  **Expand the rows:** Map over every `Row` and replace it with $N$ copies of itself.

-- If you do this, your `[String]` becomes exactly the size of the final PNG. Then, your lookup function (using `!!`) is looking at a 1:1 ratio.

-- ** Warning:** This creates a much larger list in memory. For a 256x256 grid scaled by 10, you are creating a list of ~6.5 million characters. Haskell is lazy, so it handles this reasonably well, but it is memory-heavy.

-- ### 3. The Performance Elephant in the Room (`!!`)
-- You seem to intuitively feel that calculating `list !! index` for every single pixel is inefficient. You are correct.

-- `generateImage` works by "pulling" pixels. It calls your function for calculation $W \times H$ times.
-- If your function uses `!!` (which walks from the start of the list every time), your image generation will get noticeably slow as the image gets bigger.

-- ** The "Superior" Solution:**

-- If you want to keep the code concise but fix the performance, consider converting your `[String]` into an **Array** or **Vector** just before generating the image.

-- 1.  Calculate `grid = mkGridWithRule ...`
-- 2.  Convert `grid` to an `Array (Int, Int) Char`. (Lookup: `Data.Array.listArray`).
-- 3.  Arrays allow $O(1)$ constant time lookup.
-- 4.  Pass the Array lookup to `generateImage`.

-- ### Summary of Structure

-- I recommend keeping your grid generation logic pure and small. Handle the scaling inside the drawing phase via coordinate math.

-- Your `drawAutomata` function should likely:

-- 1.  Take `ruleNum`, `gridSize`, and `scale`.
-- 2.  Generate the grid (small).
-- 3.  Convert the grid to an `Array` (to solve the indexing speed issue).
-- 4.  Call `generateImage` with size `(gridSize * scale)`.
-- 5.  Inside the lambda, use `x `div` scale` to find the array index.
-- 6.  Look up the color and return the `PixelRGB8`.

-- Okay I accept the logic and like the array idea. Now give me your version of the drawAutomata function that implements this logic.

-- Here is the implementation.

-- To make this performant and clean, we use `Data.Array`. This bridges the gap between your generation logic (which produces logical rows) and the image library (which asks for random pixel retrieval).

-- You will likely need to add `import Data.Array (listArray, (!))` to your module header or `build-depends: array` to your `.cabal` file (though `array` is standard, it is best practice to list it).

-- ```haskell
-- import Data.Array (listArray, (!))

-- -- | Drawings a cellular automata to a PNG file.
-- --
-- -- Arguments:
-- --   ruleNum: The Wolfram rule number (0-255)
-- --   size:    The logical grid dimensions (e.g., 256 for a 256x256 grid)
-- --   scale:   How many pixels wide/high one cell should be (e.g., 10)
-- --   path:    The output filename (e.g., "automata.png")
-- drawAutomata :: Int -> Int -> Int -> FilePath -> IO ()
-- drawAutomata ruleNum size scale path =
--     writePng path img
--   where
--     -- 1. Generate the logical grid (List of Strings)
--     rawGrid = mkGridWithRule ruleNum size size

--     -- 2. Convert to O(1) lookup Array
--     -- We flatten the list of strings using `concat`.
--     -- `listArray` fills the array sequentially from that flat list.
--     -- We use bounds ((0,0), (size-1, size-1)) representing ((Row, Col), (Row, Col)).
--     bounds   = ((0, 0), (size - 1, size - 1))
--     gridArr  = listArray bounds (concat rawGrid)

--     -- 3. Helper to determine Total Image Size
--     imgWidth  = size * scale
--     imgHeight = size * scale

--     -- 4. The Pixel Generator Function
--     -- generateImage passes us 'x' (column) and 'y' (row) in pixels.
--     pixelGen x y =
--         let -- Scale Logic: Integer division maps a block of pixels to one logical cell
--             logicalX = x `div` scale
--             logicalY = y `div` scale

--             -- Array Lookup: access via (Row, Col), which matches (y, x)
--             cellChar = gridArr ! (logicalY, logicalX)
--         in renderPixel cellChar

--     -- 5. Create the image
--     img = generateImage pixelGen imgWidth imgHeight
-- ```

-- ### Key Techniques Used:

-- 1.  **Coordinate Math (`div`)**: This is the "Semantic Zoom." If `scale` is 10, then pixels 0 through 9 all divide down to index 0. This creates the blocky "pixel art" look without inflating your memory usage.
-- 2.  **`listArray`**: This consumes your lazy linked list (`[String]`) once and effectively "bakes" it into a solid block of memory. This allows the `pixelGen` function to run instantly even if you generate a 4k image.
-- 3.  **Coordinate Swapping**: Notice `! (logicalY, logicalX)`. Your list of strings is organized as `[Row 0, Row 1...]`. `listArray` preserves this order. Therefore, the first index into the array is the **Row** (the Y-coordinate), and the second is the **Column** (the X-coordinate). `generateImage` provides them as X then Y, so we must swap them for the lookup.

-- I am obviously trying to make a small demonstration program of cellular automata. I expect them to look like the examples here https://plato.stanford.edu/entries/cellular-automata/supplement.html, but mine don't. Confusingly some of them look pretty good while others are way off. Do you see the logic error in my code?

-- There are two subtle but critical logic errors in your code. One explains why "some rules work and others don't" (Symmetry/Endianness), and the other explains why you might see weird drifting or artifacts at the edges (Boundary Conditions).

-- ### 1. The "Endianness" Problem (Why some rules are wrong)
-- This is the main reason your automata look different from Wolfram's examples.

-- ** The Theory:**

-- In Wolfram's notation, "Rule 30" ($00011110_2$) is defined by powers of 2.

-- *   The $2^0$ bit (the **last** bit in the binary string) corresponds to the input pattern `000` (Standard Binary 0).

-- *   The $2^7$ bit (the **first** bit in the binary string) corresponds to the input pattern `111` (Standard Binary 7).

-- ** Your Code:**

-- `myClrLst` generates patterns in counting order: `000`, `001` ... `111`.
-- `int2bin` generates the rule string with the large bit first: `bit 7`, `bit 6` ... `bit 0`.

-- When you `zip` them together, you are assigning:

-- *   Pattern `000` ($\text{int } 0$) $\rightarrow$ `Bit 7` ($\text{int } 128$)

-- *   Pattern `111` ($\text{int } 7$) $\rightarrow$ `Bit 0` ($\text{int } 0$)

-- You have completely reversed the rule definition.

-- ** Why some looked okay:**

-- Many famous rules (like Rule 90 or Rule 150) are **palindromes** in binary. If the rule is `01011010`, reversing it gives the same string, so your bug was hidden. Asymmetric rules (like Rule 110 or Rule 30) would look completely wrong.

-- ** The Fix:**

-- Simply `reverse` the binary string from `int2bin` before zipping it.

-- ```haskell
-- makeRule :: Int -> Rule
-- makeRule myrulenum =
--   let mcl = myClrLst
--       -- REVERSE added here to align Bit 0 with Pattern 000
--       blst = reverse $ int2bin myrulenum
--   in Rule $ M.fromList $ zip mcl $ map myf blst
-- ```

-- ---

-- ### 2. The "Shearing" Problem (Boundary Conditions)

-- Your `nextRow` logic handles the edges of the simulation incorrectly.

-- ```haskell
-- -- Your code:
-- colPatts = map triplet2List (zip3 rw (drop 1 rw) (drop 2 rw))
-- -- This creates a list of length (N-2).
-- -- You then patch the list back to length N using:
-- map ... (last colPatts : colPatts ++ [head colPatts])
-- ```

-- The issue is **what** you are adding back.
-- `head colPatts` represents the neighborhood of the *second* pixel in the row. You are pasting that neighbor-set onto the *last* pixel.
-- This causes information to teleport incorrectly across the screen, often creating a "shearing" diagonal drift effect across the image.

-- ** The Fix:**

-- To implement proper "Wrap Around" (Periodic Boundary Conditions), you should pad the row with its own last and first elements **before** generating the triplets.

-- Here is the corrected `nextRow`:

-- ```haskell
-- nextRow :: Rule -> Row -> Row
-- nextRow (Rule rm) rw = map (rm M.!) patterns
--   where
--     -- 1. Create a row that wraps around: [Last, ...Original..., First]
--     extendedRow = [last rw] ++ rw ++ [head rw]

--     -- 2. Zip the extended row to look at left, center, right
--     triplets = zip3 extendedRow (drop 1 extendedRow) (drop 2 extendedRow)

--     -- 3. Convert tuples to lists for the Map lookup
--     patterns = map (\(x,y,z) -> [x,y,z]) triplets
-- ```

-- With these two changes—reversing the bit list and fixing the boundary padding—your automata will exactly match the Stanford/Wolfram examples.
