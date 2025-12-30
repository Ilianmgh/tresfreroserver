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
    query = sclient.recv(1000).decode()
    rep = ""
    connexion_ended = False
    response = http_response(query)
    connexion_ended = (response != "")
    sclient.send(response)
    try :
      while not(connexion_ended) :
        query = sclient.recv(1000).decode()
        response = http_response(query)
        connexion_ended = (response != "")
        if not(connexion_ended) :
          sclient.send(response)
    except KeyboardInterrupt :
      pass
    finally :
      sclient.close()
except KeyboardInterrupt :
  pass
finally :
  serveur.close()