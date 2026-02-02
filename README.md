**TresFrerosTacoServer** is a (very) simple, minimalist web server.

# Config

You can parametrize the server via [config.py].
- `to_send_content_folder` : the folder from which the server fetches the resources. The server can provide any resource in a subfolder of this one.
- `contentlanguage`, `contenttypeofhtmlfiles` : for now, the server does not parse html file to get metadata, so it is fixed here.
- `displayable_errors` : a set of all status codes for which an html error page is available.
- `status_messages_XXX` : the message displayed on the corresponding error page.

# Methods supported

Supports POST and GET requests.

# Dynamic webpages with a ML-like language

This server has support for a ML-like language within html webpages. See `webpage_parser/documentation.pdf` for more information.

This language supports the following web features:
- Access to GET and POST parameters as global variables ;
- access to session variables ;
- SQL requests via sqlite3.

# Dependencies

## Server

A python interpreter is needed to run the server.

## ML-like intepreter

`ocaml`, `ocamlfind`, `sqlite3-ocaml`

The interpreter of the ML-like language is compiled using `ocamlc` with `ocamlfind`. The implementation of SQL request uses the ocaml package [sqlite3](https://mmottl.github.io/sqlite3-ocaml/api/sqlite3/).
