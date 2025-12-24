import time

debug : bool = False

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

def get_http_time() -> str :
  """ Return GMT time, in the format for HTTP responses """
  cur_time = time.asctime(time.gmtime()).split(' ')
  weekday, month, day, hour, year = cur_time[0], cur_time[1], cur_time[2], cur_time[3], cur_time[4]
  return f"{weekday}, {day} {month} {year} {hour} GMT"

def parse_http(text : str) -> tuple[int, str] :
  """ Parse HTTP query, returns the path to the requested file, or raise an error if malformed or unanswerable query.
      Returns (0, value) if succeeded, (1, error_str) otherwise """
  query = [x.strip('\r').split(' ') for x in text.split('\n')]
  if debug :
    print(query)
  head = query[0]
  for line in query[1:] :
    if line[0] == "Accept:" :
      print(line[1])
      if "text/html" in line[1].split(',') :
        break
      if "image/png" in line[1].split(',') :
        break
  else :
    return (1, "Client does not accept html files or favicon.")
  if len(head) != 3 or head[0] != "GET" or head[2] != "HTTP/1.1":
    raise (1, "First line of HTTP request of wrong format.")
  else :
    path = head[1]
    if not(check_path_subfolder(path)) :
      return (1, "Trying to access parent folder.")
    return (0, "."+path)

def http_response(text : str) -> bytes :
  ## Header of the response, if everything goes well TODO return bytes instead of string
  # HTTP/1.1 200 OK
  # Date : {get_http_time()}
  # Content-Type:  text/html;charset=utf-8
  # Server: Tresfreroserver
  # Content-Language: fr-FR
  # 
  try :
    errcode, parsing_res = parse_http(text)
    if errcode == 0 :
      path = parsing_res
      body = bytes_of_path(path)
      header = f"HTTP/1.1 200 OK\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body)}"
      return (header + "\n\n").encode() + body + ("\n\n\n\n").encode()
    else :
      print(text)
      print("Caused following error :")
      print(parsing_res)
      body = str_of_path("./error400.html")
      header = f"HTTP/1.1 400\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
      return (header + "\n\n" + body + "\n\n\n\n").encode()
  except IsADirectoryError :
    body = str_of_path("./error404.html")
    header = f"HTTP/1.1 404\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
    return (header + "\n\n" + body + "\n\n\n\n").encode()
  except FileNotFoundError :
    body = str_of_path("./error404.html")
    header = f"HTTP/1.1 404\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
    return (header + "\n\n" + body + "\n\n\n\n").encode()

# TODO we must treat the request for favicon :
# GET /favicon.ico undefined
# Host: localhost:9997
# User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:146.0) Gecko/20100101 Firefox/146.0
# Accept: image/avif,image/webp,image/png,image/svg+xml,image/*;q=0.8,*/*;q=0.5
# Accept-Language: en-US,en;q=0.5
# Accept-Encoding: gzip, deflate, br, zstd
# Connection: keep-alive
# Referer: http://localhost:9997/pourquoi.html
# Sec-Fetch-Dest: image
# Sec-Fetch-Mode: no-cors
# Sec-Fetch-Site: same-origin
# Priority: u=6