**TresFrerosTacoServer** is a (very) simple, minimalist web server.

# Config

You can parametrize the server via [config.py].
- `to_send_content_folder` : the folder from which the server fetches the resources. The server can provide any resource in a subfolder of this one.
- `contentlanguage`, `contenttypeofhtmlfiles` : for now, the server does not parse html file to get metadata, so it is fixed here.
- `displayable_errors` : a set of all status codes for which an html error page is available.
- `status_messages_XXX` : ignored for now. At some point, should be the message to display on a html error page.

# Methods supported

Supports POST and GET requests.

# Dynamic webpage with ML-like language

This server has support for a ML-like language within html webpages. See `webpage_parser/documentation.pdf` for more information.