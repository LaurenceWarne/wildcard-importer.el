# wildcard-importer.el

Import stuff like wildcards you aren't always prompted to with LSP.

## Installation

## Usage

Update `wildcard-importer-alist` until satisfied like so:

```elisp
(setq wildcard-importer-alist
      (wildcard-importer-with-extensions
       '((scala-mode ("import foo.bar" . "tips on when to use foo.bar")))))
```

(In your `dir-locals.el` would probably be best).  And then invoke: `M-x wildcard-importer-import`.
