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
