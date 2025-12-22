module Automata where

import Numeric (showIntAtBase)
import Data.Char (intToDigit,digitToInt)
import Data.List (foldl')

int2bin :: Int -> String
int2bin myint =
  showIntAtBase 2 intToDigit myint ""

bin2int :: String -> Int
bin2int =
  foldl' (\ accum dig -> accum * 2 + digitToInt dig ) 0 

Don't write it, but give me a short overview of how to go about writing tests for these two functions that verify the property of their being inverses.



# Testing Inverse Properties for Binary Conversion Functions

Here's a strategic approach to testing that `int2bin` and `bin2int` are inverses:

## Key Testing Strategies

### 1. **Round-trip Property Testing**
The fundamental property: `bin2int (int2bin n) == n` for valid inputs.

- Test with **non-negative integers** (since binary representation of negatives isn't standard)
- Use property-based testing (QuickCheck) to generate random non-negative integers
- The property should hold for all `n >= 0`

### 2. **Edge Cases to Consider**
- **Zero**: Does `bin2int (int2bin 0)` return `0`?
- **Powers of 2**: Clean boundaries like 1, 2, 4, 8, 16, etc.
- **Large numbers**: Ensure it scales properly

### 3. **The Reverse Direction**
Testing `int2bin (bin2int s) == s` is trickier because:
- Not all strings are valid binary strings
- Leading zeros complicate equality (e.g., "101" vs "0101")

**Solution**: Either:
- Only test with *valid* binary strings (no leading zeros except "0")
- Or normalize the comparison to ignore leading zeros

### 4. **Recommended Test Structure**
```haskell
-- Property: round-trip from Int
prop_roundTripInt :: NonNegative Int -> Bool

-- Unit tests for specific values
testZero, testPowersOfTwo, testRandomSamples :: Test

-- Property: round-trip from binary string (with normalization)
prop_roundTripBinary :: ValidBinaryString -> Bool
```

This approach ensures mathematical correctness while handling the asymmetry in the domain/codomain of these functions.

Okay. Let's see a minimum version of the .hs file that is a proper test and show me how to integrate it into the project file.


# Test File

Create `test/AutomataSpec.hs`:

```haskell
module AutomataSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Automata

spec :: Spec
spec = do
  describe "int2bin and bin2int" $ do
    
    it "converts 0 correctly" $ do
      int2bin 0 `shouldBe` "0"
      bin2int "0" `shouldBe` 0
    
    it "converts some specific values correctly" $ do
      int2bin 5 `shouldBe` "101"
      bin2int "101" `shouldBe` 5
      int2bin 255 `shouldBe` "11111111"
      bin2int "11111111" `shouldBe` 255
    
    it "round-trips from Int to binary and back" $ 
      property $ \(NonNegative n) -> 
        bin2int (int2bin n) == n
    
    it "round-trips from binary string and back (normalized)" $
      property $ \(NonNegative n) -> n > 0 ==>
        let binStr = int2bin n
        in int2bin (bin2int binStr) == binStr
```

# Project Integration

Update your `.cabal` file to add a test suite section:

```cabal
name:                your-project-name
version:             0.1.0.0
-- ... other fields ...

library
  exposed-modules:     Automata
  build-depends:       base >=4.7 && <5
  hs-source-dirs:      src
  default-language:    Haskell2010

test-suite spec
  type:                exitcode-stdio-1.0
  main-is:             Spec.hs
  other-modules:       AutomataSpec
  hs-source-dirs:      test
  build-depends:       base >=4.7 && <5
                     , your-project-name
                     , hspec
                     , QuickCheck
  default-language:    Haskell2010
  build-tool-depends:  hspec-discover:hspec-discover
```

# Test Runner

Create `test/Spec.hs`:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

# Running Tests

```bash
cabal test
# or with stack:
stack test
```

This is the minimal setup using `hspec` (for structure) and `QuickCheck` (for property-based testing), which are the standard testing libraries in Haskell. The `hspec-discover` automatically finds all `*Spec.hs` files.

Do I still need the AutomataSpec.hs? And why is QuickCheck not lower case like other packages?

# Good Questions!

## 1. Yes, you still need `AutomataSpec.hs`

The `Spec.hs` file with `hspec-discover` is just a **test runner** that automatically finds and runs all your test modules. The actual tests live in `AutomataSpec.hs`.

**How it works:**
- `hspec-discover` scans the `test/` directory at compile time
- It finds all modules ending in `Spec` (like `AutomataSpec`)
- It generates code to run all their `spec` functions
- This saves you from manually maintaining a list of imports

**Without hspec-discover**, your `Spec.hs` would look like:
```haskell
import Test.Hspec
import qualified AutomataSpec

main :: IO ()
main = hspec $ do
  AutomataSpec.spec
  -- ... manually add each new spec module
```

## 2. QuickCheck capitalization

`QuickCheck` is uppercase because that's literally the package name chosen by its authors back in the early 2000s. It's a proper noun/brand name.

Many older Haskell packages use CamelCase:
- `QuickCheck`
- `HUnit` 
- `ByteString`

While newer packages tend toward lowercase:
- `hspec`
- `aeson`
- `text`

There's no technical reason—just historical convention and author preference. In your `.cabal` file's `build-depends`, you must use the exact package name as published on Hackage.
