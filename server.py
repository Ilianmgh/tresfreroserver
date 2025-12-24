from socket import socket
from http import http_response

file : str = "<!DOCTYPE html><html><head><title>Test</title><meta charset='utf-8' /></head><body><h1>Titre</h1><p>Ceci est un paragraphe.</p></body></html>"
bidule = f"HTTP/1.1 200 OK\nServer: nginx/1.10.3 (ubuntu)\nDate: Tue, 23 December 2025 14:18:18 GMT\nContent-Type: text/html\nContent-Lenght: {len(file)}\nLast-Modified : Tue, 23 December 2025 14:18:18 GMT\n\n{file}\n\n"

serveur = socket()
serveur.bind(('0.0.0.0', 9998))
serveur.listen()

try :
  while True :
    (sclient, adclient) = serveur.accept()
    donnee = sclient.recv(1000).decode()
    rep = ""
    sclient.send(http_response(donnee))
    try :
      while True :
        donnee = sclient.recv(1000).decode()
        sclient.send(http_response(donnee))
        print(donnee)
    except KeyboardInterrupt :
      pass
    sclient.close()
except KeyboardInterrupt :
  pass
serveur.close()