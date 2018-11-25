# lambda-calculus-presentation

This is the code behind my slides for the Lambda Pi Meetup talks about the Lambda Calculus.

## How does it work?
The slides are written in Emacs org-mode. That's were most of the code (except some additional tests) resides, too.

The Makefile uses a "headless" emacs to export (tangle) the code and to export the slides as Tex files. Then the tests are executed with stack and the Tex files are build with XeTeX.

This technique is called Literate Programming. It makes sure that the code examples in documents compile and can be tested.

Because this setup needs some dependencies (Tex, Emacs, Graphviz, ...) I added a Nix file. Using the Nix package manager I can now easily setup the needed environment on different machines.

The `init.el` file does the same for the "Emacs World". Because every Emacs installation differs, this file provides the minimal needed config to build the project and installs the needed packages.

## How to use it?

Most likely you attended to the Lambda Pi Meetup and want to play around with the code. In this case you can ignore all the Emacs related stuff and regard this project a an ordinary `stack project`:

```
stack build
stack test
```

If you want to use the Emacs exporting and slide creation stuff, you can use Nix and `make`:
```
nix-shell
make
```

Beware: The Tex build stage isn't rock solid! If there was a XeTex error, you'll likely need to run:
```
make clean
```

## Warning
This "publishing pipeline" for Literate Programming "works for me". I didn't create it to be a general solution. There are some rough edges and if you want to use it you should know `make`, Nix, Emacs and stack.

## Bugs

The evaluator (`UntypedEval`) doesn't care about ambiguous variable / parameter names.

For example:

(λx. λy. x + y) y

There are two ys in the expression. Reducing the expression to  (λy. y + y) would be wrong, because it looses the information of the outer y. Reducing the expression to (λy. x + y) would be wrong, because x is now free (even though it was previously bound) and we lose the information of y.

There are different solutions to this problem:
- Do a renaming phase before any evaluation to get rid of the ambiguities.
- De Bruijn Indices - A clever technique that replaces the variable names with an index that describes where they were bound.

Kudos & Thanks to @pheymann for spotting this bug!
