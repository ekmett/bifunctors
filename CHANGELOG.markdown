5.6.2 [2024.03.19]
------------------
* Support building with `template-haskell-2.22.*` (GHC 9.10).

5.6.1 [2023.03.13]
------------------
* Provide instances for the `Swap` and `Assoc` type classes from the `assoc`
  package. (These instances were previously defined in `assoc` itself, but they
  have been migrated over to `bifunctors` in tandem with the `assoc-1.1`
  release.)
* Only depend on `bifunctor-classes-compat` if building with GHC 8.0.

5.6 [2023.03.12]
----------------
* Drop support for GHC 7.10 and earlier.
* Move the `Data.Bifunctor`, `Data.Bifoldable`, and `Data.Bitraversable`
  compatibility modules to the new `bifunctor-classes-compat` package. For
  backwards compatibility, the `bifunctors` library re-exports
  `Data.Bifoldable` and `Data.Bitraversable` modules from
  `bifunctor-classes-compat` when building with GHC 8.0.

  If your library depends on `bifunctors` and compiles with pre-8.2
  versions of GHC, be warned that it may be possible to construct a
  build plan involving a pre-`5.6` version of `bifunctors` where:

  * Some of the `Bifunctor` instances come from
    `bifunctor-classes-compat`'s compatibility classes, and
  * Other `Bifunctor` instances come from `bifunctors`'s compatibility classes.

  These compatibility classes are distinct, so this could lead to build errors
  under certain conditions. Some possible ways to mitigate this risk include:

  * Drop support for GHC 8.0 and older in your library.
  * Require `bifunctors >= 5.6` in your library.
  * If neither of the options above are viable, then you can temporarily
    define instances for the old compatibility classes from `bifunctors` like
    so:

    ```hs
    -- For Bifunctor instances
    import qualified "bifunctor-classes-compat" Data.Bifunctor as BifunctorCompat
    #if !MIN_VERSION_bifunctors(5,6,0) && !MIN_VERSION_base(4,8,0)
    import qualified "bifunctors" Data.Bifunctor as Bifunctor
    #endif

    instance BifunctorCompat.Bifunctor MyType where ...

    #if !MIN_VERSION_bifunctors(5,6,0) && !MIN_VERSION_base(4,8,0)
    instance Bifunctor.Bifunctor MyType where ...
    #endif
    ```

    ```hs
    -- For Bifoldable and Bitraversable instances
    import qualified "bifunctor-classes-compat" Data.Bifoldable as BifoldableCompat
    import qualified "bifunctor-classes-compat" Data.Bitraversable as BitraversableCompat
    #if !MIN_VERSION_bifunctors(5,6,0) && !MIN_VERSION_base(4,10,0)
    import qualified "bifunctors" Data.Bifoldable as Bifoldable
    import qualified "bifunctors" Data.Bitraversable as Bitraversable
    #endif

    instance BifoldableCompat.Bifoldable MyType where ...
    instance BitraversableCompat.Bitraversable MyType where ...

    #if !MIN_VERSION_bifunctors(5,6,0) && !MIN_VERSION_base(4,10,0)
    instance Bifoldable.Bifoldable MyType where ...
    instance Bitraversable.Bitraversable MyType where ...
    #endif
    ```

  If your package does nothing but define instances of `Bifunctor` _et al._,
  you may consider replacing your `bifunctors` dependency with
  `bifunctor-classes-compat` to reduce your dependency footprint. If you do,
  it is strongly recommended that you bump your package's major version number
  so that your users are alerted to the details of the migration.
* Define a `Foldable1` instance for `Joker`, and define `Bifoldable1` instances
  for `Biff`, `Clown`, `Flip`, `Join`, `Joker`, `Product`, `Tannen`, and
  `WrappedBifunctor`. These instances were originally defined in the
  `semigroupoids` library, and they have now been migrated to `bifunctors` as
  a side effect of adapting to
  [this Core Libraries Proposal](https://github.com/haskell/core-libraries-committee/issues/9),
  which adds `Foldable1` and `Bifoldable1` to `base`.

5.5.15 [2023.02.27]
-------------------
* Support `th-abstraction-0.5.*`.

5.5.14 [2022.12.07]
-------------------
* Define `Functor`, `Foldable`, and `Traversable` instances for `Sum` and
  `Product`.

5.5.13 [2022.09.12]
-------------------
* Make the `Biapplicative` instances for tuples lazy, to match their `Bifunctor`
  instances.

5.5.12 [2022.05.07]
-------------------
* Backport an upstream GHC change which removes the default implementation of
  `bitraverse`. Per the discussion in
  https://github.com/haskell/core-libraries-committee/issues/47, this default
  implementation was completely broken, as attempting to use it would always
  result in an infinite loop.

5.5.11 [2021.04.30]
-------------------
* Allow building with `template-haskell-2.18` (GHC 9.2).

5.5.10 [2021.01.21]
-------------------
* Fix a bug in which `deriveBifoldable` could generate code that triggers
  `-Wunused-matches` warnings.

5.5.9 [2020.12.30]
------------------
* Explicitly mark modules as Safe or Trustworthy.

5.5.8 [2020.10.01]
------------------
* Fix a bug in which `deriveBifunctor` would fail on sufficiently complex uses
  of rank-n types in constructor fields.
* Fix a bug in which `deriveBiunctor` and related functions would needlessly
  reject data types whose two last type parameters appear as oversaturated
  arguments to a type family.

5.5.7 [2020.01.29]
------------------
* Add `Data.Bifunctor.Biap`.

5.5.6 [2019.11.26]
------------------
* Add `Category`, `Arrow`, `ArrowChoice`, `ArrowLoop`, `ArrowZero`, and
  `ArrowPlus` instances for `Data.Bifunctor.Product`.

5.5.5 [2019.08.27]
------------------
* Add `Eq{1,2}`, `Ord{1,2}`, `Read{1,2}`, and `Show{1,2}` instances for data
  types in the `Data.Bifunctor.*` module namespace where possible. The
  operative phrase is "where possible" since many of these instances require
  the use of `Eq2`/`Ord2`/`Read2`/`Show2`, which are not available when
  built against `transformers-0.4.*`.

5.5.4 [2019.04.26]
------------------
* Support `th-abstraction-0.3` or later.
* Don't incur a `semigroup` dependency on recent GHCs.

5.5.3 [2018.07.04]
------------------
* Make `biliftA2` a class method of `Biapplicative`.
* Add the `traverseBia`, `sequenceBia`, and `traverseBiaWith` functions for
  traversing a `Traversable` container in a `Biapplicative`.
* Avoid incurring some dependencies when using recent GHCs.

5.5.2 [2018.02.06]
------------------
* Don't enable `Safe` on GHC 7.2.

5.5.1 [2018.02.04]
------------------
* Test suite fixes for GHC 8.4.

5.5 [2017.12.07]
----------------
* `Data.Bifunctor.TH` now derives `bimap`/`bitraverse`
  implementations for empty data types that are strict in the argument.
* `Data.Bifunctor.TH` no longer derives `bifoldr`/`bifoldMap` implementations
  that error on empty data types. Instead, they simply return the folded state
  (for `bifoldr`) or `mempty` (for `bifoldMap`).
* When using `Data.Bifunctor.TH` to derive `Bifunctor` or `Bitraversable`
  instances for data types where the last two type variables are at phantom
  roles, generated `bimap`/`bitraverse` implementations now use `coerce` for
  efficiency.
* Add `Options` to `Data.Bifunctor.TH`, along with variants of existing
  functions that take `Options` as an argument. For now, the only configurable
  option is whether derived instances for empty data types should use the
  `EmptyCase` extension (this is disabled by default).

5.4.2
-----
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
