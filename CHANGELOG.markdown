next
----
* Make `deriveBitraversable` use `liftA2` in derived implementations of `bitraverse` when possible, now that `liftA2` is a class method of `Applicative` (as of GHC 8.2)
* Backport slightly more efficient implementations of `bimapDefault` and `bifoldMapDefault`

5.4.1
-----
* Add explicit `Safe`, `Trustworthy`, and `Unsafe` annotations. In particular, annotate the `Data.Bifoldable` module as `Trustworthy` (previously, it was inferred to be `Unsafe`).

5.4
---
* Only export `Data.Bifoldable` and `Data.Bitraversable` when building on GHC < 8.1, otherwise they come from `base`
* Allow TH derivation of `Bifunctor` and `Bifoldable` instances for datatypes containing unboxed tuple types

5.3
---
* Added `bifoldr1`, `bifoldl1`, `bimsum`, `biasum`, `binull`, `bilength`, `bielem`, `bimaximum`, `biminimum`, `bisum`, `biproduct`, `biand`, `bior`, `bimaximumBy`, `biminimumBy`, `binotElem`, and `bifind` to `Data.Bifoldable`
* Added `Bifunctor`, `Bifoldable`, and `Bitraversable` instances for `GHC.Generics.K1`
* TH code no longer generates superfluous `mempty` or `pure` subexpressions in derived `Bifoldable` or `Bitraversable` instances, respectively

5.2.1
----
* Added `Bifoldable` and `Bitraversable` instances for `Constant` from `transformers`
* `Data.Bifunctor.TH` now compiles warning-free on GHC 8.0

5.2
-----
* Added several `Arrow`-like instances for `Tannen` so we can use it as the Cayley construction if needed.
* Added `Data.Bifunctor.Sum`
* Added `BifunctorFunctor`, `BifunctorMonad` and `BifunctorComonad`.
* Backported `Bifunctor Constant` instance from `transformers`

5.1
---
* Added `Data.Bifunctor.Fix`
* Added `Data.Bifunctor.TH`, which permits `TemplateHaskell`-based deriving of `Bifunctor`, `Bifoldable` and `Bitraversable` instances.
* Simplified `Bitraversable`.

5
-
* Inverted the dependency on `semigroupoids`. We can support a much wider array of `base` versions than it can.
* Added flags

4.2.1
-----
* Support `Arg` from `semigroups` 0.16.2
* Fixed a typo.

4.2
---
* Bumped dependency on `tagged`, which is required to build cleanly on GHC 7.9+
* Only export `Data.Bifunctor` when building on GHC < 7.9, otherwise it comes from `base`.

4.1.1.1
-------
* Added documentation for 'Bifoldable' and 'Bitraversable'

4.1.1
-----
* Added `Data.Bifunctor.Join`
* Fixed improper lower bounds on `base`

4.1.0.1
-------
* Updated to BSD 2-clause license

4.1
---
* Added product bifunctors

4.0
---
* Compatibility with `semigroupoids` 4.0

3.2
---
* Added missing product instances for `Biapplicative` and `Biapply`.

3.1
-----
* Added `Data.Biapplicative`.
* Added the `Clown` and `Joker` bifunctors from Conor McBride's "Clowns to the left of me, Jokers to the right."
* Added instances for `Const`, higher tuples
* Added `Tagged` instances.

3.0.4
-----
* Added `Data.Bifunctor.Flip` and `Data.Bifunctor.Wrapped`.

3.0.3
---
* Removed upper bounds from my other package dependencies
