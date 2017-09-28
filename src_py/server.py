import configparser
from os.path import basename

from erlang_python import ErlangPythonServices
from tfimpl import Model
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer
from thrift.transport import TSocket
from thrift.transport import TTransport


class Dbg(object):
    active = False


DEBUGGER = Dbg()


# --------------------------------------------------------------------------
# Python services
# --------------------------------------------------------------------------

class PythonServicesHandler:
    def __init__(self):
        self.clients = dict()
        self.model = None


    def add_activation(self,
                       client_id,
                       activation):
        if DEBUGGER.active:
            print(
                "{} - client {} - add_activation (activation={}): Start".format(basename(__file__),
                                                                                client_id,
                                                                                activation))

        self.model = self.clients[client_id]

        self.model.add_activation(activation)

        self.clients[client_id] = self.model


    def add_data_chunk(self,
                       client_id,
                       data_chunk,
                       labels_chunk,
                       scale_chunk):
        if DEBUGGER.active:
            print(("{} - client {} - add_data_chunk (data_chunk={}, labels_chunk={}, "
                   + "scale_chunk={}): Start").format(basename(__file__), client_id, data_chunk,
                                                      labels_chunk, scale_chunk))

        self.model = self.clients[client_id]

        if not scale_chunk:
            scale = None
        else:
            scale = scale_chunk

        self.model.add_data_chunk(data_chunk,
                                  labels_chunk,
                                  scale)

        self.clients[client_id] = self.model


    def add_layer(self,
                  client_id,
                  layer_outputs):
        if DEBUGGER.active:
            print("{} - client {} - add_layer (layer_outputs={}): Start".format(basename(__file__),
                                                                                client_id,
                                                                                layer_outputs))

        self.model = self.clients[client_id]

        self.model.add_layer(layer_outputs)

        self.clients[client_id] = self.model


    def get_weights(self,
                    client_id, ):
        if DEBUGGER.active:
            print("{} - client {} - get_weights (): Start".format(basename(__file__), client_id)),

        self.model = self.clients[client_id]

        return self.model.get_weights()


    def initialize(self,
                   client_id,
                   num_inputs,
                   num_outputs,
                   learning_rate):
        if DEBUGGER.active:
            print(
                (
                    "{} - client {} - initialize (num_inputs={}, " +
                    "num_outputs={}, learning_rate={}): Start").format(basename(__file__),
                                                                       client_id,
                                                                       num_inputs, num_outputs,
                                                                       learning_rate))

        self.model = Model()
        self.model.initialize(num_inputs,
                              num_outputs,
                              learning_rate)

        self.clients[client_id] = self.model


    def predict(self,
                client_id,
                data):
        if DEBUGGER.active:
            print("{} - client {} - predict (data={}): Start".format(basename(__file__), client_id,
                                                                     data))

        self.model = self.clients[client_id]

        prediction = self.model.predict(data)

        self.clients[client_id] = self.model

        return prediction


    def set_cost(self,
                 client_id,
                 cost):
        if DEBUGGER.active:
            print("{} - client {} - set_cost (cost={}): Start".format(basename(__file__), client_id,
                                                                      cost))

        self.model = self.clients[client_id]

        self.model.set_cost(cost)

        self.clients[client_id] = self.model


    def set_learning_rate(self,
                          client_id,
                          learning_rate):
        if DEBUGGER.active:
            print("{} - client {} - set_learning_rate (learning_rate={}): Start".format(
                basename(__file__),
                client_id,
                learning_rate))

        self.model = self.clients[client_id]

        self.model.set_learning_rate(learning_rate)

        self.clients[client_id] = self.model


    def set_weights(self,
                    client_id,
                    new_weights):
        if DEBUGGER.active:
            print(
                "{} - client {} - set_weights (new_weights={}): Start".format(basename(__file__),
                                                                              client_id,
                                                                              new_weights))

        self.model = self.clients[client_id]

        self.model.set_weights(new_weights)

        self.clients[client_id] = self.model


    def terminate_model(self,
                        client_id):
        if DEBUGGER.active:
            print(
                "{} - client {} - terminate_model (): Start".format(basename(__file__), client_id))

        del self.clients[client_id]

        if DEBUGGER.active:
            print(
                "{} - client {} - terminate_model (): active models {}".format(basename(__file__),
                                                                               client_id,
                                                                               len(self.clients)))


    def train(self,
              client_id,
              epochs,
              batch_size):
        if DEBUGGER.active:
            print("{} - client {} - train (epochs={}, batch_size={}): Start".format(
                basename(__file__), client_id, epochs, batch_size))

        self.model = self.clients[client_id]

        result = self.model.train(epochs,
                                  batch_size)

        self.clients[client_id] = self.model

        return result


# --------------------------------------------------------------------------
# Starting the server
# --------------------------------------------------------------------------

if __name__ == "__main__":
    if DEBUGGER.active:
        print("{} - __main__ (): Start".format(basename(__file__)))

    # --------------------------------------------------------------------------
    # Read network parameters
    # --------------------------------------------------------------------------

    config = configparser.ConfigParser()
    config.read("server.ini")
    try:
        host = config["NETWORK ADDRESS"]["host"]
    except KeyError:
        host = "127.0.0.1"
    try:
        port = int(config["NETWORK ADDRESS"]["port"])
    except KeyError:
        port = 8778

    # --------------------------------------------------------------------------
    # Configuring server
    # --------------------------------------------------------------------------

    handler = PythonServicesHandler()
    processor = ErlangPythonServices.Processor(handler)
    transport = TSocket.TServerSocket(host=host, port=port)
    tfactory = TTransport.TBufferedTransportFactory()
    pfactory = TBinaryProtocol.TBinaryProtocolFactory()

    server = TServer.TThreadedServer(processor, transport, tfactory, pfactory)

    # --------------------------------------------------------------------------
    # Running server
    # --------------------------------------------------------------------------

    print(("{} - __main__ (): This server with ip address {} and port number {} " +
           "will keep running until you interrupt the program " +
           "with Ctrl-C").format(basename(__file__), host, port))
    print("...")

    server.serve()

    if DEBUGGER.active:
        print("{} - __main__ (): Done".format(basename(__file__)))
