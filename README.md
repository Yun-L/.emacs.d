# Emacs Config

#### Dependencies

* git
* [Input fonts](https://input.djr.com) (Input mono, sans, serif)


#### How it works

Emacs will load `init.el` on startup and `init.el` will compile all the configurations from `config.org` into `config.el`. `config.el` is then loaded as valid emacs lisp.

Changes to configs should only be in `config.org` because all manual changes to `config.el` will get overwritten everytime `config.org` gets recompiled.
