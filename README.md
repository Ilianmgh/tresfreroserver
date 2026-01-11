**TresFrerosTacoServer** is a (very) simple, minimalist web server.

# Config

You can parametrize the server via [config.py].
- `to_send_content_folder` : the folder from which the server fetches the resources. The server can provide any resource in a subfolder of this one.
- `contentlanguage`, `contenttypeofhtmlfiles` : for now, the server does not parse html file to get metadata, so it is fixed here.
- `displayable_errors` : a set of all status codes for which an html error page is available.
- `status_messages_XXX` : ignored for now. At some point, should be the message to display on a html error page.

# Methods supported

For now, only supports GET requests.