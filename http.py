import time
import os
import utils
import config
from threading import Lock
from typing import Any, Callable, Iterable

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

def get_webpage(path : str, generate_session_id_mut : Lock, arguments : None | str = None, session_id : None | str = None) -> tuple[str, bytes] :
  """ Returns as bytes the content of the file given by [path]. Also returns a session id if one was assigned when evaluating the page, the empty string otherwise. """
  if not hasattr(get_webpage, 'session') :
    get_webpage.session : dict[str, dict[str, str]] = {}
  path_split = path.split(".")
  potential_new_session_id : str = ""
  extension : str = path_split[-1]
  name : str = path[:len(path) - len(extension) - 1]
  res : bytes
  if extension == "htmlml" :
    str_args : str = ""
    if arguments is not None :
      str_args = f"-argstr \"{arguments}\""
    session_args : str = ""
    if session_id is not None and session_id in get_webpage.session :
      session_args = f"-argrepr \"{utils.url_encoding("SESSION", get_webpage.session[session_id])}\""
    produce_page_command_line : str = f"./webpage_parser/produce_page.x {path} {name}.html {str_args} {session_args}"
    print("Executing...", produce_page_command_line)
    os.system(produce_page_command_line)
    print("") # For a newline after subprocess call
    path = f"{name}.html"
    with open(path, mode = "r") as f :
      first_line = f.readline()
      if session_id is None :
        session_id = utils.generate_session_id(generate_session_id_mut)
        potential_new_session_id = session_id
      # assert session id is string (?)
      if session_id not in get_webpage.session :
        get_webpage.session[session_id] = {}
      utils.parse_url_dictionary(first_line.strip("\n\t\r"), get_webpage.session[session_id]) # TODO add TTL to session-recorded values
      if debug :
        print(f"data received from ML page: {first_line}")
        print(f"current session: {get_webpage.session[session_id]}")
      res = f.read().encode()
  else :
    with open(path, mode = "rb") as f :
      res = f.read()
  return (potential_new_session_id, res)

## HTTP request manager

def get_content_type(path : str) -> tuple[int, str] :
  """ Returns (error code, the content type of the file designated by [path]) by examining the extension.
      error code is 0 if OK, 1 if the path is not well formatted, 2 if the format is not supported.
      Format supported : htmlml, html, css, js, png, svg, webp, avif, ico """
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
    if extension == "css" :
      return (0, config.contenttypeofcss)
  return (2, "")

def parse_http(text : str) -> tuple[int, dict[str, Any] | str] :
  """ Parse HTTP query.
      Returns (0, value) if succeeded, (1, error_str) otherwise
      For now, valid [value]s can only be a file from a GET request """
  data : dict[str, Any] = {}
  query = [x.strip('\r').split(' ') for x in text.split('\n')]
  if debug :
    print("DEBUG, query, space-separated, line by line :")
    print(query)
    print("END OF DEBUG")
  head = query[0]
  head[0] = head[0].lower()
  head[2] = head[2].lower()
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
      line[0] = line[0].lower()
      if line[0] == "accept:" :
        # for now, [accept]ed formats must be comma separated, won't parse correctly if separated by [, ].
        # for now, ignoring quality values [;q=...]
        if debug :
          print("DEBUG :", line[1], "now as a list : ", line[1].split(','), "END OF DEBUG")
        data["accept"] = [utils.strip_quality_values(x) for x in line[1].split(',') if utils.strip_quality_values(x) is not None]
      if line[0] == "connection:" :
        line[1] = line[1].lower()
        if line[1] == "close" :
          data["keep-alive"] = False
        elif line[1] == "keep-alive" :
          data["keep-alive"] = True
      if line[0] == "content-length:" :
        data["content-length"] = line[1].lower() # TODO make it so it does not crash when the request doesn't provide a proper integer
      if line[0] == "content-type:" :
        data["content-type"] = line[1].lower()
      if line[0] == "cookie:" :
        data["cookie"] = utils.parse_cookies(line[1:])
    return (0, data)

## HTTP response manager

def get_http_time() -> str :
  """ Return GMT time, in the format for HTTP responses """
  cur_time = time.asctime(time.gmtime()).split(' ')
  weekday, month, day, hour, year = cur_time[0], cur_time[1], cur_time[2], cur_time[3], cur_time[4]
  return f"{weekday}, {day} {month} {year} {hour} GMT"

def is_accepted(contenttype : str, contentaccepted : Iterable[str]) -> bool :
  content_stripped = utils.strip_quality_values(contenttype)
  if content_stripped is None :
    raise Exception("contenttype not of the right format.")
  content_data = content_stripped.split("/")
  if len(content_data) != 2 :
    if debug :
      print("Content Type :", contenttype, "of different format than [something/somethingelse]")
    return False
  type, format = content_data[0].lower(), content_data[1].lower()
  for content in contentaccepted :
    accepted_stripped = utils.strip_quality_values(content)
    if accepted_stripped is not None :
      if debug :
        print(f"DEBUG: accepted field {content} is ill-formatted")
      accepted_data = accepted_stripped.split("/")
      if len(accepted_data) != 2 :
        if debug :
          print("Content Type :", content, "of different format than [something/somethingelse]")
      else :
        accepted_type, accepted_format = accepted_data[0].lower(), accepted_data[1].lower()
        if (accepted_type == "*" or type == accepted_type) and (accepted_format == "*" or accepted_format == format) :
          return True
  return False

def make_body(status : int, generate_session_id_mut : Lock, path : str = "", additional_data : str | None = None, additional_message : str | None = None, session_id : str | None = None) -> tuple[int, tuple[str, bytes]] :
  """ If [status] is a supported error, fetches the appropriate error page.
      If [status] is 200, tries to fetch the page from the [config.to_send_content_folder] folder.
      Returns the updated status and the body e.g. 404 instead of 200 if the page is not in the folder. """
  if status == 200 :
    try :
      if additional_data is not None :
        return (200, get_webpage(path, generate_session_id_mut, additional_data, session_id=session_id))
      else :
        return (200, get_webpage(path, generate_session_id_mut, session_id=session_id))
    except (IsADirectoryError, FileNotFoundError) as e :
      status = 404
  if status in config.displayable_errors :
    arguments : str
    if additional_message is not None :
      arguments = f"SERVER&status={status}&text={config.get_status_message(status)}&additional={additional_message}"
    else :
      arguments = f"SERVER&status={status}&text={config.get_status_message(status)}&additional=" # Ugly fix for now, change when able to check for defined variables in ML
    return (status, get_webpage(f"./error.htmlml", generate_session_id_mut, arguments))
  return (500, get_webpage(f"./error.htmlml", generate_session_id_mut, f"SERVER&status={status}&text=bonjour&additional={additional_message}"))

def make_header(status : int, contenttype : str, language : str, contentlength : int, cookies : dict[str, tuple[str, dict[str, str]]]) -> str :
  """ Produces the HTTP header according to the informations passed as argument.
    [cookies] is a dictionary from cookies' names to cookies' data e.g. {"mycookie" : ("42", {"Secure": True, "SameSite": "Strict"})} will result in the line: Set-Cookie: mycookie=42; Secure; SameSite=Strict """
  first_line = f"HTTP/1.1 {status}"
  if status == 200 :
    first_line += " OK"
  contenttype_line = "Content-Type: " + contenttype
  language_line = "Content-Language: " + language
  contentlength_line = "Content-Length: " + str(contentlength)
  cookies_lines = ""
  for cookie_name in cookies :
    value, cookie_params = cookies[cookie_name]
    cookie_line = f"Set-Cookie: {cookie_name}={value}"
    for param in cookie_params :
      if param.lower() == "domain" :
        cookie_line += f"; Domain={cookie_params[param]}"
      if param.lower() == "expires" :
        cookie_line += f"; Expires={cookie_params[param]}"
      if param.lower() == "HttpOnly" :
        cookie_line += f"; HttpOnly"
      if param.lower() == "max-age" :
        cookie_line += f"; Max-Age={cookie_params[param]}"
      if param.lower() == "partitioned" :
        cookie_line += f"; Partitioned"
      if param.lower() == "path" :
        cookie_line += f"; Path={cookie_params[param]}"
      if param.lower() == "samesite" :
        cookie_line += f"; SameSite={cookie_params[param]}"
      if param.lower() == "Secure" :
        cookie_line += f"; Secure"
    cookies_lines += (cookie_line + "\r\n")
  return first_line + "\r\n" + contenttype_line + "\r\n" + language_line + "\r\n" + contentlength_line + "\r\n" + cookies_lines

def http_response(text : str, fetch_n_bytes : Callable[[int], bytes], generate_session_id_mut : Lock) -> tuple[bool, bytes] :
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
  cur_session_id : str | None = None
  to_send_cookies : dict[str, tuple[str, dict[str, str]]] = {}
  keep_alive : bool = True # HTTP/>=1.1 : Connection:keep-alive is the default value 
  additional_data : str | None = None
  if errcode == 0 :
    assert(isinstance(info, dict))
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
      # Testing accordance with 'Accept' field
      if "accept" in info :
        if not(is_accepted(utils.extract_content_type(contenttype), info["accept"])) :
          if debug :
            print("DEBUG resource not accepted by client :")
            print(contenttype, "stripped :", utils.extract_content_type(contenttype), "\nWhereas client only accepts", info["accept"])
            print(utils.extract_content_type(contenttype))
            print("END OF DEBUG")
          status = 400
      # Retrieving optional additional information in url for get or in payload of post requests
      if info["method"] == "post" :
        try :
          info["post_data"] = fetch_n_bytes(int(info["content-length"])).decode()
        except ValueError :
          print(f"ValueError when trying to get post data {info}") # Don't know why we catch this error
      if "get_url_data" in info :
        additional_data = f"GET&{info["get_url_data"]}"
      elif ("post_data" in info and "content-type" in info and info["content-type"] == "application/x-www-form-urlencoded") :
        additional_data = f"POST&{info["post_data"]}"
      # Deciding whether to keep the connection up
      if "keep-alive" in info :
        keep_alive_stored = info["keep-alive"]
        assert(isinstance(keep_alive_stored, bool))
        keep_alive = keep_alive_stored
      if "cookie" in info :
        if "sessionId" in info["cookie"] :
          cur_session_id = info["cookie"]["sessionId"]
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
  status, (new_session_id, body) = make_body(status, generate_session_id_mut, path, additional_data, session_id=cur_session_id)
  if new_session_id != "" :
    to_send_cookies["sessionId"] = (new_session_id, {"SameSite": "Strict"})
  if status in config.displayable_errors :
    contenttype = config.contenttypeofhtmlfiles
  header : bytes = make_header(status, contenttype, config.contentlanguage, len(body), to_send_cookies).encode()
  return (keep_alive, header + "\r\n\r\n".encode() + body + "\r\n\r\n".encode())