# psfmt
PureScript formatter

# Usage

For formatting every `.purs` file in the current directory:

```
psfmt
```

For formatting every `.purs` file in specific directories:

```
psfmt src test
```

For formatting a specific file:

```
psfmt File.purs
```

# Philosophy
## No configuration

The formatter shouldn't have configuration options. One standard for all, no
bikeshedding.

## Reduce diffs

Make formatting such that a code change changes the minimal lines of code in a
git diff.
