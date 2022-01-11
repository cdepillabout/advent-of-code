#!/usr/bin/env bash
#
# This script takes a Day*Generated.hs as input, and uses it to create a
# Day*Real.hs file.
#
# This is necessary because Coq is not able to add things like extra imports
# during extraction.  See
# https://coq.inria.fr/library/Coq.extraction.ExtrHaskellString.html
# for example.

# Add required language extensions
cat << 'EOF' > Day01Real.hs
{-# LANGUAGE StandaloneDeriving #-}
EOF

# Get everything up to and including the qualified import of Prelude.
# from the Coq-generated file.
head -n 6 ./Day01Generated.hs >> Day01Real.hs

# Add extra headers we need.
cat << 'EOF' >> Day01Real.hs
import qualified Data.Bits
import qualified Data.Char
EOF

# Add the rest of the generated file.
tail -n +7 ./Day01Generated.hs >> Day01Real.hs

# Add any additional code required.
cat << 'EOF' >> Day01Real.hs

-- instance Prelude.Show Positive

instance Prelude.Show N where
  show = n_to_string

main = do
  -- let inputStr ="199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
  inputStr <- Prelude.readFile "input-day01"
  Prelude.print (solve inputStr)
EOF
