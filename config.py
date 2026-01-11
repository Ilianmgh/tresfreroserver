## Resources information

to_send_content_folder = "/var/www/html"

contentlanguage = "fr" # TODO For now is the only value. At some point, must fetch it from the html page we send or default to this one
contenttypeofhtmlfiles = "text/html;charset=utf-8" # TODO Same as for contentlanguage : For now is the only value. At some point, must fetch it from the html page we send or default to this one

## Status messages

status_message_100 : list[str] = []
status_message_200 : list[str] = []
status_message_300 : list[str] = []
status_message_400 : list[str] = ["Mauvaise requếte.", "Non autorisé.", "", "Accès interdit.", "Page non trouvée."]
status_message_500 : list[str] = []

displayable_errors = set([400, 401, 403, 404, 500])

def get_status_message(status : int) -> str :
  if 100 <= status < 200 :
    return status_message_100[status - 100]
  if 200 <= status < 300 :
    return status_message_200[status - 200]
  if 300 <= status < 400 :
    return status_message_300[status - 300]
  if 400 <= status < 500 :
    return status_message_400[status - 400]
  if 500 <= status < 600 :
    return status_message_500[status - 500]
  return ""