import socketserver
import json

from tfimpl import Model

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    pass

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The RequestHandler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """

    def handle(self):
        # self.request is the TCP socket connected to the client
        print("Connection received from {}".format(self.client_address[0]))
        model = Model()
        while True:
            msglen = int.from_bytes(read_exact(self.request, 4), 'big')
            data = read_exact(self.request, msglen)
            print("Msg length:", len(data))
            message = json.loads(data)
            operation = message['operation']
            arguments = message['args']
            resp = call(model, operation, arguments)
            resplen = len(resp)
            # Send the response back to the client
            respmsg = resplen.to_bytes(4, 'big') + resp
            print("Response:", respmsg)
            self.request.sendall(respmsg)

def read_exact(socket, size):
    data = b''
    while len(data) < size:
        buffer = socket.recv(size - len(data))
        print("received: ", buffer)
        if not buffer:
            raise ValueError('Connection closed')
        data += buffer
    return data

def call(model, operation, arguments):
    result = getattr(model, operation)(*arguments)
    print("Function call result", result)
    if operation != "predict":
        # TODO: We don't care for the result of anything that is not predict.
        result = "ok"
    return json.dumps({'result': result}).encode('utf-8')

if __name__ == "__main__":
    # Create the server, binding to any interface on port 8778
    server = ThreadedTCPServer(('', 8778), MyTCPHandler)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()
