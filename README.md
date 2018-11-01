
This is an unofficial version of the OCaml Standard Library docs but with a 
modernized theme.  [Click here](https://www.streamingspring.com/ocaml/docs/) for where the guide is hosted.

It parses the official html docs, converts it into json to store it.  Then it 
decodes the json and builds a modern site around it using better structure with a sidebar to quickly move between modules and functions.

It uses the following libraries:

From the OCaml opam libraries:

- lambdasoup
- atdgen

From the Bucklescript npm libraries:

- bucklescript-tea
- bs-json

# Install

1. Clone the repo:
```
git clone https://github.com/seadynamic8/ocaml-modern-docs
```

2. Download a copy of the official docs from: https://caml.inria.fr/pub/docs/manual-ocaml/index.html and store it in ocaml-modern-docs/parser directory.

So it should look like this:

- ocaml-modern-docs/parser/htmlman/...
- ocaml-modern-docs/parser/htmlman/libref/...

3. Build the parser (make sure to have `dune` installed already)

```
cd parser
dune build parser.exe
dune exec ./parser.exe
```

This will output a `modules.json` file in the `parser` directory.

4. Decode the json and build the site
```
cd ..  -- Back into the ocaml-modern-docs directory
npm install
npm run build
```

5. Launch - If you have `parcel` and `npm-run-all` installed

```
npm run start
```

## Notes

- It's still a work in progress, but should be useful (It's just documentation)

- The code needs some refactoring, especially the parser, so bear that in mind.

- There was some hacks to get the decoding working because I couldn't figure
out how to easily modify to the output for variants in atdgen and bs-json doesn't
have a more flexible way to parse the json for variants not in the preferred
format.

- It was inspired by Elixir docs.

- Not going to be actively maintained.

## License

MIT