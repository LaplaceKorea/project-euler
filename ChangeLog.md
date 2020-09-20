# Changelog for project-euler

## Version 0.1.1.0
- updated to GHC 8.10.2
- code formatting

## Version 0.1.0.2
- renamed `primeFactorPairs` to `primePowerDecomposition`
- optimized `totient` function
- corrected `pandigital0` and `pandigital1`
- repllaced uses of `array` with `vector`
- rely only on `Data.Tuple.Extra` instead of `Control.Arrow`
- hardcoded the big number in q008
- replaced uses of `Monad (->)` with `Applicative (->)`
- questions 62, 63, 64, 65, 66, and 70 complete
- removed questions outside "safe-for-publication" range (1-100)

## Version 0.1.0.1
- moved `pascal` (Polynomials -> NumbersExtra) and removed duplicate `choose`
- updated .gitignore to exclude recent problems

## Version 0.1.0.0
- Questions 1-61 and 709