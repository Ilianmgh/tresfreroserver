import config
from typing import Any
from socket import socket
from http import http_response
import threading

debug : bool = True
thread_debug : bool = False

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

def get_n_bytes(sock : socket, n : int) -> bytes :
  """ Reads [n] bytes from [sock]. Blocking call, will loop until [n] bytes where read """
  data : bytes = b""
  n_bytes_read = 0
  while n_bytes_read < n :
    bytes = sock.recv(n - n_bytes_read)
    n_bytes_read += len(bytes)
    data += bytes
  return data

def manage_connection(sclient : socket, adclient : Any, generate_session_id_mut : threading.Lock) -> None :
  if thread_debug :
    print(f"{threading.get_ident()} is starting")
  with pool_sem :
    if thread_debug :
      print(f"{threading.get_ident()} now exchanging messages")
    keep_alive = True
    while keep_alive and not(server_interrupt.is_set()) :
      if debug :
        print(f"thread n°{threading.get_ident()}: Waiting for msg...")
      query : str = get_http_query(sclient)
      if debug :
        print(f"thread n°{threading.get_ident()}: Received sth...")
      if query != "" :
        if debug :
          print(f"thread n°{threading.get_ident()}: QUERY STARTS ON NEXT LINE")
          print(query)
          print(f"thread n°{threading.get_ident()}: QUERY ENDS FROM PREV LINE")
        keep_alive, response = http_response(query, lambda n : get_n_bytes(sclient, n), generate_session_id_mut)
        sclient.send(response)
      else :
        keep_alive = False
    if debug :
      print(f"thread n°{threading.get_ident()}: Closing connection with current client...")
    sclient.close()
  if thread_debug :
    print(f"{threading.get_ident()} now exiting")

thread_pool : list[threading.Thread] = []
pool_sem = threading.Semaphore(value = max(config.max_simultaneous_connections, 0))
server_interrupt = threading.Event()
generate_session_id_mut = threading.Lock()

server = socket()
server.bind(('0.0.0.0', 9999))
server.listen()

try :
  while True :
    (sclient, adclient) = server.accept()
    if config.max_simultaneous_connections < 0 :
      pool_sem.release()
    t = threading.Thread(target=manage_connection, args=(sclient, adclient, generate_session_id_mut))
    t.start()
except KeyboardInterrupt :
  server_interrupt.set()
  for t in thread_pool :
    t.join()
finally :
  server.close()