#!/usr/bin/env bash

# Get everything up to the qualified import of Prelude.
head -n 6 ./Day01Generated.hs > Day01Real.hs

# Add extra headers we need.
echo "import qualified Data.Bits" >> Day01Real.hs
echo "import qualified Data.Char" >> Day01Real.hs

# Add the rest of the generated file.
tail -n +7 ./Day01Generated.hs >> Day01Real.hs

# Add any additional code required.
cat << 'EOF' >> Day01Real.hs
main =
  Prelude.print
    ( parse
        parseToken
        "GET / HTTP/1.1"
      :: Prelude.Either
          (Prelude.Maybe Prelude.String)
          (Prelude.String, Prelude.String)
    )
EOF
