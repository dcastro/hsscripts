# Haskell Scripts

A bunch of assorted Haskell stack scripts.

## DirectDeps.hs

> Usage: stack DirectDeps.hs \<project-path\> [-s|--stack-path \<stack-path\>]

Outputs a list of the direct dependencies of each component of a stack project.

NB: `stack list-dependencies`, on the other hand, when asked for the dependencies of a test component, will output all dependencies of [_all_ test components](https://github.com/commercialhaskell/stack/issues/3695)).

```plain
$ git clone git@github.com:dcastro/hsscripts.git
$ stack DirectDeps.hs ./hsscripts

[
  {
    "packageName": "hsscripts",
    "components": [
      {
        "target": "hsscripts",
        "deps": [
          "Cabal",
          "aeson",
          ...
        ]
      },
      {
        "target": "hsscripts:exe:hsscripts-exe",
        "dependsOnLib": true,
        "deps": [
          "base",
          "hsscripts"
        ]
      },
      {
        "target": "hsscripts:test:hsscripts-test",
        "dependsOnLib": true,
        "deps": [
          "base",
          "hsscripts"
        ]
      }
    ]
  }
]
```

## More to come!