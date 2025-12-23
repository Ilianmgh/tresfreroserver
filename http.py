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

def get_http_time() -> str :
  """ Return GMT time, in the format for HTTP responses """
  cur_time = time.asctime(time.gmtime()).split(' ')
  weekday, month, day, hour, year = cur_time[0], cur_time[1], cur_time[2], cur_time[3], cur_time[4]
  return f"{weekday}, {day} {month} {year} {hour} GMT"

def parse_http(text : str) -> str :
  """ Parse HTTP query, returns the path to the requested file, or raise an error if malformed or unanswerable query. """
  query = [x.strip('\r').split(' ') for x in text.split('\n')]
  if debug :
    print(query)
  head = query[0]
  for line in query[1:] :
    if line[0] == "Accept:" :
      if "text/html" in line[1].split(',') :
        break
  else :
    raise ValueError("Client does not accept html files.")
  if len(head) != 3 or head[0] != "GET" or head[2] != "HTTP/1.1":
    raise (ValueError("First line of HTTP request of wrong format."))
  else :
    path = head[1]
    if not(check_path_subfolder(path)) :
      raise (ValueError("Trying to access parent folder."))
    return "."+path

def http_response(text : str) -> str :
  ## Header of the response, if everything goes well
  # HTTP/1.1 200 OK
  # Date : {get_http_time()}
  # Content-Type:  text/html;charset=utf-8
  # Server: Tresfreroserver
  # Content-Language: fr-FR
  # 
  try :
    path = parse_http(text)
    body = str_of_path(path)
    header = f"HTTP/1.1 200 OK\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
  except IsADirectoryError :
    body = str_of_path("./error404.html")
    header = f"HTTP/1.1 404\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
  except FileNotFoundError :
    body = str_of_path("./error404.html")
    header = f"HTTP/1.1 404\nDate : {get_http_time()}\nContent-Type:  text/html;charset=utf-8\nServer: Tresfreroserver\nContent-Language: fr-FR\nContent-Length: {len(body.encode('utf-8'))}"
  return header + "\n\n" + body + "\n\n\n\n"

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