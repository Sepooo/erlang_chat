import socket
import threading

def receive_messages(sock):
    """Riceve i messaggi dal server e li stampa."""
    while True:
        try:
            data = sock.recv(1024).decode('utf-8')
            if not data:
                print("Connessione chiusa dal server.")
                break
            print(f"\n[Server]: {data}")
        except ConnectionResetError:
            print("Connessione persa con il server.")
            break
        except Exception as e:
            print(f"Errore durante la ricezione: {e}")
            break

def send_messages(sock):
    """Permette all'utente di inviare messaggi al server."""
    while True:
        try:
            message = input("> ")
            sock.sendall(message.encode('utf-8'))
        except Exception as e:
            print(f"Errore durante l'invio: {e}")
            break

def main():
    host = 'localhost'
    port = 8080

    try:
        # Creazione del socket e connessione al server
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((host, port))
        print(f"Connesso al server {host}:{port}")

        # Avvio dei thread per ricevere e inviare messaggi
        receive_thread = threading.Thread(target=receive_messages, args=(sock,))
        receive_thread.daemon = True
        receive_thread.start()

        send_thread = threading.Thread(target=send_messages, args=(sock,))
        send_thread.daemon = True
        send_thread.start()

        # Attesa che uno dei thread termini
        receive_thread.join()
        send_thread.join()

    except ConnectionRefusedError:
        print(f"Impossibile connettersi al server {host}:{port}. Il server è in ascolto?")
    except Exception as e:
        print(f"Errore: {e}")
    finally:
        sock.close()
        print("Connessione chiusa.")

if __name__ == "__main__":
    main()
