# 0.3.4

* Fix a regression in 0.3.3, which made `liftA2` too strict.

# 0.3.3

* Speed up `Applicative` / `Alternative` / `MonadPlus` instances.
* Speed up `(>>=)`: add a trick to check whether a callback is `const []`.

# 0.3.2

* Add `MonadFail` instance.

# 0.3.1

* Add `Alternative` instance.

# 0.3

* Add `MonadPlus` instance.

# 0.2

* Change `diagonal`.

# 0.1

* Initial release.
