# Macrodynamics

[![Build Status](https://travis-ci.org/DalekBaldwin/macrodynamics.svg?branch=master)](https://travis-ci.org/DalekBaldwin/macrodynamics)

Macrodynamics is a language extension that broadens the notion of dynamic scope inside macroexpansion code. Macrodynamic bindings are scoped not just to the code called during an individual expansion, but also to subsequent expansions of the code returned within that dynamic scope. In short, you can write macros that behave as if they were expanded recursively instead of iteratively.

This goes a long way toward rectifying a major limitation of Common Lisp, described in detail [here](http://qiita.com/guicho271828/items/07ba4ff11bff494dc03f). However, macrodynamics only handles dynamic bindings meant to be send information downstream from where they are bound; it does not provide an analogue of the condition system or continuations to transmit information upstream.

## Usage

You can declare macrodynamic variables with `def-dynenv-var`, a cognate of `defvar`. These variables can be left unbound or given a top-level, global value.

```lisp
(def-dynenv-var **my-macrodynamic-var** value)
(def-dynenv-var **my-unbound-macrodynamic-var**)
```

(You may want to use a special notational convention for macrodynamic variables and functions. I prefer `**double-earmuffs**` myself.)

For macrodynamic functions, `def-dynenv-fun` works like `defun`, while `def-unbound-dynenv-fun` only takes a name which remains unbound at the top-level. (There's just no way to sensibly combine both syntaxes within one macro.)

```lisp
(def-dynenv-fun **my-macrodynamic-fun** (&rest stuff) (do-stuff-with stuff))
(def-unbound-dynenv-fun **my-unbound-macrodynamic-fun**)
```

Macrodynamic variables and functions live in a separate namespace from regular Lisp variables and functions. To establish bindings for them, you must choose one of a few dynamic-only compile-time variants of familiar operators. `ct-let` and `ct-let*` bind variables, and `ct-flet` and `ct-labels` bind functions. You can also retrieve bound functions as values using `dynenv-function` instead of `function` or `#'`.

To define macros that need to read or bind macrodynamic entities within the dynamic scope of their expander code, you can use `def-dynenv-macro`:

```lisp
(def-dynenv-macro some-macro (&body body)
  `(do-stuff
       ,(do-something)
     ,(ct-let ((**a-macrodynamic-variable**
                (non-destructively-augment **a-macrodynamic-variable**)))
        `(progn ,@body))))

;; this function will signal an error if not called within the dynamic scope
;; of a macrodynamic macro's expansion
(defun do-something ()
  (generate-code-with **a-macrodynamic-variable**))
```

`def-dynenv-macro` is just a convenience macro that can extract the macrodynamic context from the lexical environment regardless of whether you include an `&environment` parameter. Alternatively, you can explicitly pass an environment to `with-dynenv` at the top of a macro's body (or at least surrounding any forms that need to bind or reference macrodynamic entities). This makes it easier to integrate macrodynamics with any other special macro-defining-macros you might want to use.

```lisp
(def-special-macro-using-some-other-macro-library some-macro (&body body &environment env)
  (with-dynenv env
    `(do-stuff
         ,(do-something)
       ,(ct-let ((**a-macrodynamic-variable**
                  (non-destructively-augment **a-macrodynamic-variable**)))
          `(progn ,@body)))))
```

This library is meant to be used in a purely functional manner, and it will signal an error if you attempt to set, rather than bind, a macrodynamic entity. That's right, dynamic scope is compatible with functional programming; it just admits a slightly looser definition of referential transparency. You can think of dynamic variables as an implicit set of additional arguments passed to every function. When dynamic bindings are in play, a function called with the same arguments may not always return the same results, but a function called at the top-level with the same arguments always will. What this means for macrodynamics is that an entire top-level form will always have the same macroexpansion. Normally, this is all you really care about, since you spend most of your time reasoning about top-level forms that you can see in their entirety.

One drawback is that you won't always be able to use SLIME's `C-c C-m` to verify what a non-toplevel expression will expand into, but this is a limitation you already accept any time you use `macrolet` or `symbol-macrolet`. Macrodynamics are no more dangerous than lexically-bound macros; in fact, they're just an abstraction layer built on top of `symbol-macrolet`.

## But Why?

What is dynamic from the perspective of expander code is lexical from the perspective of expanded code. When you take full advantage of this semantic duality, it's easy to lexically scope, and thus make more composable, certain implementation concerns that do not easily map onto lexical runtime variables. In the most common case, you can bind compile-time metadata about how a given variable is meant to be interpreted by another macro that may or may not be used further down the syntax tree.

Instead of placing your trust in a code walker like macroexpand-dammit, macrodynamics piggybacks on your implementation's built-in macroexpansion facility. When you use a code walker, you introduce a potential point of failure that can screw up the expansion of code in between the macro that establishes a compile-time dynamic context and the macro that uses it. With macrodynamics, you can trust your Lisp implementation to correctly expand any in-between macros that were written without any knowledge of this language extension.
