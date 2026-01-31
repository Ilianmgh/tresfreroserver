import time
import os
import utils
import config # TODO replace page per error by a single page and config giving status messages to be replaced in said page
from typing import Any, Dict, Iterable

debug : bool = False

## File manager

def check_path_subfolder(path : str) -> bool :
  for folder in path.split('/') :
    if folder == ".." :
      return False
  return True

def str_of_path(path : str) -> str :
  """ Returns as a string the content of the file given by [path] """
  res = ""
  with open(path, mode = "r") as f :
    for line in f :
      res += line
  return res

def bytes_of_path(path : str) -> bytes :
  """ Returns as bytes the content of the file given by [path] """
  res : bytes
  with open(path, mode = "rb") as f :
    res = f.read()
  return res

def get_webpage(path : str, arguments: None | str = None) -> bytes :
  """ Returns as bytes the content of the file given by [path] """
  path_split = path.split(".")
  extension : str = path_split[-1]
  name : str = path[:len(path) - len(extension) - 1]
  if extension == "htmlml" :
    str_args : str = ""
    if arguments is not None :
      str_args = arguments
    os.system(f"./webpage_parser/produce_page.x {path} {name}.html \"{str_args}\"")
    print("") # For a newline after subprocess call
    path = f"{name}.html"
  with open(path, mode = "rb") as f :
    res = f.read()
  return res

## HTTP request manager

def get_content_type(path : str) -> tuple[int, str] :
  """ Returns (error code, the content type of the file designated by [path]) by examining the extension.
      error code is 0 if OK, 1 if the path is not well formatted, 2 if the format is not supported.
      Format supported : html and png, svg, webp, avif, ico """
  if path[0] == "." :
    path = path[1:]
  file_info = path.split(".")
  if debug :
    print("DEBUG :")
    print(path)
    print(file_info)
    print("END OF DEBUG")
  if len(file_info) != 2 :
    return (1, "")
  else :
    extension = file_info[1]
    if debug :
      print("DEBUG :")
      print(extension)
      print("END OF DEBUG")
    if extension == "ico" :
      extension = "png"
    if extension in ["png", "svg", "webp", "avif"] :
      return (0, "image/" + extension)
    if extension == "html" or extension == "htmlml":
      return (0, config.contenttypeofhtmlfiles)
    if extension == "js" :
      return (0, config.contenttypeofjsscripts)
  return (2, "")

def parse_http(text : str) -> tuple[int, Dict[str, Any] | str] :
  """ Parse HTTP query.
      Returns (0, value) if succeeded, (1, error_str) otherwise
      For now, valid [value]s can only be a file from a GET request """
  data : Dict[str, Any] = {}
  query = [x.strip('\r').split(' ') for x in text.lower().split('\n')]
  if debug :
    print("DEBUG, query, space-separated, line by line :")
    print(query)
    print("END OF DEBUG")
  head = query[0]
  # We except a message of the form [(GET|POST) path HTTP/1.1\n...]
  if len(head) != 3 or (head[0] != "get" and head[0] != "post") or head[2] != "http/1.1" :
    return (1, f"First line of HTTP request of wrong format : {head}")
  else :
    # Setting the method
    data["method"] = head[0]
    # Registering the parameters in the url
    url = head[1].split("?")
    assert(len(url) <= 2)
    if len(url) == 2 :
      data["get_url_data"] = url[1]
    path = url[0]
    if not(check_path_subfolder(path)) :
      return (1, "Trying to access parent folder.")
    # All checks on path done, adding path to dictionary
    data["path"] = config.to_send_content_folder + path
    # Checking out other needed informations
    for line in query[1:] :
      if line[0] == "accept:" :
        # for now, [accept]ed formats must be comma separated, won't parse correctly if separated by [, ].
        # for now, ignoring quality values [;q=...]
        if debug :
          print("DEBUG :", line[1], "now as a list : ", line[1].split(','), "END OF DEBUG")
        data["accept"] = [utils.strip_quality_values(x) for x in line[1].split(',') if utils.strip_quality_values(x) is not None]
      if line[0] == "connection:" :
        if line[1] == "close" :
          data["keep-alive"] = False
        elif line[1] == "keep-alive" :
          data["keep-alive"] = True
      if line[0] == "content-length:" :
        data["content-length"] = line[1] # TODO make it so it does not crash when the request doesn't provide a proper integer
    return (0, data)

## HTTP response manager

def get_http_time() -> str :
  """ Return GMT time, in the format for HTTP responses """
  cur_time = time.asctime(time.gmtime()).split(' ')
  weekday, month, day, hour, year = cur_time[0], cur_time[1], cur_time[2], cur_time[3], cur_time[4]
  return f"{weekday}, {day} {month} {year} {hour} GMT"

def is_accepted(contenttype : str, contentaccepted : Iterable[str]) -> bool :
  content_stripped = utils.strip_quality_values(contenttype)
  content_data = content_stripped.split("/")
  if len(content_data) != 2 :
    if debug :
      print("Content Type :", contenttype, "of different format than [something/somethingelse]")
    return False
  type, format = content_data[0].lower(), content_data[1].lower()
  for content in contentaccepted :
    accepted_stripped = utils.strip_quality_values(content)
    accepted_data = accepted_stripped.split("/")
    if len(accepted_data) != 2 :
      if debug :
        print("Content Type :", content, "of different format than [something/somethingelse]")
    else :
      accepted_type, accepted_format = accepted_data[0].lower(), accepted_data[1].lower()
      if (accepted_type == "*" or type == accepted_type) and (accepted_format == "*" or accepted_format == format) :
        return True
  return False

def make_body(status : int, path : str = "", additional_data : str | None = None, additional_message : str | None = None) -> tuple[int, bytes] :
  """ If [status] is a supported error, fetches the appropriate error page.
      If [status] is 200, tries to fetch the page from the [config.to_send_content_folder] folder.
      Returns the updated status and the body e.g. 404 instead of 200 if the page is not in the folder. """
  assert(additional_message is None) # TODO at some point, should be added to the error page.
  if status == 200 :
    try :
      if additional_data is not None :
        return (200, get_webpage(path, additional_data))
      else :
        return (200, get_webpage(path))
    except (IsADirectoryError, FileNotFoundError) as e :
      status = 404
  if status in config.displayable_errors :
    return (status, get_webpage(f"./error{status}.html"))
  return (500, get_webpage(f"./error500.html"))

def make_header(status : int, contenttype : str, language : str, contentlength : int) -> str :
  first_line = f"HTTP/1.1 {status}"
  if status == 200 :
    first_line += " OK"
  contenttype_line = "Content-Type: " + contenttype
  language_line = "Content-Language: " + language
  contentlength_line = "Content-Length: " + str(contentlength)
  return first_line + "\r\n" + contenttype_line + "\r\n" + language_line + "\r\n" + contentlength_line

def http_response(text : str, fetch_n_bytes) -> tuple[bool, bytes] :
  """ Returns a tuple [(keep_alive, response)] of :
      - a boolean indicating whether to keep the TCP connection alive after answering.
      - the http response to the request [text]. """
  if text.strip() == "" : # Not sure if that's useful
    return (False, "".encode())
  errcode, info = parse_http(text)
  path : str
  status : int
  body : bytes
  contenttype : str
  keep_alive : bool
  if errcode == 0 :
    path = info["path"]
    if path == (config.to_send_content_folder + "/") :
      path = config.to_send_content_folder + "/" + config.default_page
    elif path == (config.to_send_content_folder + "/favicon.ico") :
      path = config.to_send_content_folder + "/favicon.png"
    errcode_contenttype, contenttype = get_content_type(path)
    if debug :
      print("DEBUG content type :")
      print(contenttype)
      print("END OF DEBUG")
    if errcode_contenttype == 0 :
      status = 200
      if "accept" in info :
        if not(is_accepted(utils.strip_content_type(contenttype), info["accept"])) :
          if debug :
            print("DEBUG resource not accepted by client :")
            print(contenttype, "stripped :", utils.strip_content_type(contenttype), "\nWhereas client only accepts", info["accept"])
            print(utils.strip_content_type(contenttype))
            print("END OF DEBUG")
          status = 400
    elif errcode_contenttype == 1 :
      status = 400
    elif errcode_contenttype == 2 :
      status = 500
  else :
    if debug :
      print("DEBUG : The following HTTP query...")
      print(text)
      print("... caused following error :")
      print(info)
      print("END OF DEBUG")
    path = ""
    status = 400
  if info["method"] == "post" : # TODO check "accept" field before computing with ML
    try :
      info["post_data"] = fetch_n_bytes(int(info["content-length"]))
    except ValueError :
      pass 
  body : bytes
  if "get_url_data" in info :
    status, body = make_body(status, path, f"GET&{info["get_url_data"]}")
  elif "post_data" in info :
    status, body = make_body(status, path, f"POST&{info["post_data"]}")
  else :
    status, body = make_body(status, path)
  if status in config.displayable_errors :
    contenttype = config.contenttypeofhtmlfiles
  header : bytes = make_header(status, contenttype, config.contentlanguage, len(body)).encode()
  if "keep-alive" in info :
    keep_alive = info["keep-alive"]
  else :
    keep_alive = True # HTTP/>=1.1 : Connection:keep-alive is the default value 
  return (keep_alive, header + "\r\n\r\n".encode() + body + "\r\n\r\n".encode())