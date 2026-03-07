**TresFrerosTacoServer** is a (very) simple, minimalist web server.

# Config

You can parametrize the server via [config.py].
- `to_send_content_folder` : the folder from which the server fetches the resources. The server can provide any resource in a subfolder of this one.
- `contentlanguage`, `contenttypeofhtmlfiles` : for now, the server does not parse html file to get metadata, so it is fixed here.
- `displayable_errors` : a set of all status codes for which an html error page is available.
- `status_messages_XXX` : the message displayed on the corresponding error page.

# Methods supported

Supports POST and GET requests.

# Dynamic webpages with TresML language

This server has support for TresML, a ML-like functional language within webpages with extension `.tml`. See `TresML/Documentation/documentation.pdf` for more information.

For instance, this language supports the following web features:
- Access to GET and POST parameters as global variables ;
- access to session variables ;
- SQL requests via sqlite3 ;
- HTTP redirection.

# Dependencies

## Server

A python interpreter is needed to run the server.

## TresML intepreter

`ocaml`, `ocamlfind`, `sqlite3-ocaml`

The interpreter of TresML requires an ocaml compiler and `ocamlfind`. The implementation of SQL requests uses the ocaml package [sqlite3](https://mmottl.github.io/sqlite3-ocaml/api/sqlite3/).
