# cl-chat
> A Common Lisp LLM chat library

Usage
------

`cl-chat` is available via [ocicl](https://github.com/ocicl/ocicl).  Install it like so:
```
$ ocicl install chat
```

This repo also includes a web-chat interface.  Run it like so:
```
$ sbcl --eval "(asdf:load-system :web-chat)" --eval "(web-chat:start-server)"
```

![alt text](static/images/web-chat.png "web-chat UI")

Related Projects
-----------------

Related projects include:
* [cl-completions](https://github.com/atgreen/cl-completions) an LLM completions library
* [cl-embeddings](https://github.com/atgreen/cl-embeddings) an LLM embeddings library
* [cl-chroma](https://github.com/atgreen/cl-chroma) for a Lisp interface to the [Chroma](https://www.trychroma.com/) vector database.

Author and License
-------------------

``cl-chat`` was written by [Anthony
Green](https://github.com/atgreen) and is distributed under the terms
of the MIT license.
