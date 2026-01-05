from socket import socket
from http import http_response

debug : bool = True

file : str = "<!DOCTYPE html><html><head><title>Test</title><meta charset='utf-8' /></head><body><h1>Titre</h1><p>Ceci est un paragraphe.</p></body></html>"
bidule = f"HTTP/1.1 200 OK\nServer: nginx/1.10.3 (ubuntu)\nDate: Tue, 23 December 2025 14:18:18 GMT\nContent-Type: text/html\nContent-Lenght: {len(file)}\nLast-Modified : Tue, 23 December 2025 14:18:18 GMT\n\n{file}\n\n"

serveur = socket()
serveur.bind(('0.0.0.0', 9999))
serveur.listen()

# TODO add a buffer and split http header when encoutering a METHOD, otherwise request for page and favicon gets concatenated...

try :
  while True :
    (sclient, adclient) = serveur.accept()
    keep_alive = True
    while keep_alive :
      query = sclient.recv(1000).decode()
      if debug :
        print(query)
      keep_alive, response = http_response(query)
      sclient.send(response)
    if debug :
      print("Closing connection with current client...")
    sclient.close()
except KeyboardInterrupt :
  serveur.close()