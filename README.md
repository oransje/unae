# unae

Untyped arithmetic expressions interpreter as describe by Benjamin Pierce

## Setup

This projects uses stack tool for project management, ensures it is installed

* `stack build` -> Compiles the project;
* `stack ruin` -> TODO: Run a cli that accepts untyped arithmetic expressions as valids;
* `stack build --test` -> builds and run tests;

## Testing

This projects runs using HSpec for testing pipeline, the `Main.hs` entrypoint discover all other files with HsSpecDiscover, effectively searching for all files with Spec suffix inside the test folder.

The respective files:
```
test
├── LexerSpec.hs
├── Main.hs
├── ParserSpec.hs
└── Spec.hs
```

## Usage and example

TODO

## Credits

TODO: Cite Benjamin Pierce, stack and lepluire and boluut
