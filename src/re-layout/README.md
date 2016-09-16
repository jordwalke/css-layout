re-layout:
==========
Reimplementation of CSS layout using pure Reason.

## Building and Installing
```sh
cd src/re-layout
npm install
```

## Rebuilding
```sh
npm run reasonbuild
```
## Generating the source files

The instructions above merely build the source files, some of which were
auto-generated.  Auto-generating involves running `grunt transpile` in
`../../`, but you must have the special branch of `Reason` and `rejs` build in
directories on your `~/Desktop` (yes, your desktop) for now. Then you will
have to manually fix a bunch of type errors. Hopefully we won't need to
regenerate the files ever again and can manually begin taking over the
`Layout.re` file by hand.
