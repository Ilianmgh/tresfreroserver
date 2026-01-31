from socket import socket
from http import http_response

debug : bool = True

serveur = socket()
serveur.bind(('0.0.0.0', 9999))
serveur.listen()

http_methods : list[str] = ["get", "head", "options", "trace", "put", "delete", "post", "patch", "connect"]

def is_first_line_of_http(line : str) -> bool :
  data : list[str] = line.split(" ")
  if len(data) == 3 :
    if data[0].lower() in http_methods :
      http_version = data[2].lower().split("/") # should be ["HTTP", "X.Y"] or simply "X".
      if http_version[0] == "HTTP" :
        return True
  return False

def buffered_readline(sock : socket) -> str :
  """ Returns one line read from [sock].
    Returns the empty string if the connection is terminated by sender. """
  line : str = ""
  one_char : str = sock.recv(1).decode()
  while one_char != "\n" and one_char != "" :
    line += one_char
    one_char = sock.recv(1).decode()
  return line

def get_http_query(sock : socket) -> str :
  """ Returns _one_ full HTTP query, as a string, received from [sock].
    Will only read one HTTP query even if several were sent via [sock] before calling this function.
    If the connection is closed from the client's side, returns the empty string.
    This function is blocking, it will wait until a full HTTP query is received from [sock], or until the connection is terminated by the client. """
  query : str = ""
  line : str = buffered_readline(sock).strip("\n\t\r ")
  while line :
    query += line + "\n"
    line = buffered_readline(sock).strip("\n\t\r ")
  return query

def get_n_bytes(sock : socket, n : int) -> str :
  """ Reads [n] bytes from [sock]. Blocking call, will loop until [n] bytes where read """
  query : str = ""
  n_bytes_read = 0
  while n_bytes_read < n :
    bytes = sock.recv(n)
    n_bytes_read += len(bytes)
    query += bytes.decode()
  return query

try :
  while True :
    (sclient, adclient) = serveur.accept()
    keep_alive = True
    while keep_alive :
      if debug :
        print("Waiting for msg...")
      query : str = get_http_query(sclient)
      if debug :
        print("Received sth...")
      if query != "" :
        if debug :
          print("QUERY STARTS ON NEXT LINE")
          print(query)
          print("QUERY ENDS FROM PREV LINE")
        keep_alive, response = http_response(query, lambda n : get_n_bytes(sclient, n))
        sclient.send(response)
      else :
        keep_alive = False # TODO investigate if this have the right semantic. Firefox seems to send empty queries after some minutes of inactivity. We may want to keep the session open in these cases.
    if debug :
      print("Closing connection with current client...")
    sclient.close()
except KeyboardInterrupt :
  serveur.close()