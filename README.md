# Data.Maybe.Preserve

"Preserve" Maybe helpers. Few functions to make some kind of `guard`s.
See [this source file](src/Data/Maybe/Preserve.hs) for details.

# Usage

This isn't published anywere yet.

You can use it by adding these lines to `extra-deps` section of `stack.yaml`:

```haskell
-- stack.yaml
extra-deps:
  - git: https://github.com/unclechu/haskell-data-maybe-preserve
    commit: 21bda604bcdca32bbd73d51d50e8cea1ba61904e
```

And add `data-maybe-preserve` to your package dependencies:

```haskell
-- package.yaml
dependencies:
  - base
  - data-maybe-preserve
```

# Author

Viacheslav Lotsmanov

# License

[MIT](LICENSE)
