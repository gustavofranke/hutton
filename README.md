# hutton

This is my sandbox repo for playing around with the concepts, ideas, and most importantly, code, from [this absolutely amazing book](http://www.cs.nott.ac.uk/~pszgmh/pih.html).

You can buy it from [here](https://www.cambridge.org/ie/academic/subjects/computer-science/programming-languages-and-applied-logic/programming-haskell-2nd-edition) or [here](https://www.amazon.co.uk/Programming-Haskell-Graham-Hutton/dp/1316626229/).

It contains 120 exercises, so for me, this is still work in progress.

The project was setup using the following commands:
```
stack new hutton simple
cd hutton/
stack setup
```

The cycle for building and testing is:
```
stack test --fast --haddock-deps --file-watch
```

Build the Hoogle search index with the following command:
```
stack hoogle -- generate --local
```

To start the Hoogle local server:
```
stack hoogle -- server --local --port=8080
```

Hie file generation:
First install the tool,
```
stack build --copy-compiler-tool implicit-hie
stack exec -- which implicit-hie
/Users/gustavo/.stack/compiler-tools/x86_64-osx/ghc-8.8.4/bin/gen-hie
```

Generate the file:
```
gen-hie > hie.yaml
```

All stack commands were ripped off from [this great blog post](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
