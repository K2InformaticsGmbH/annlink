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
        self.model = None

    def add_activation(self,
                       activation):
        if DEBUGGER.active:
            print(
                "{} - add_activation (activation={}): Start".format(basename(__file__), activation))

        self.model.add_activation(activation)

    def add_data_chunk(self,
                       data_chunk,
                       labels_chunk,
                       scale_chunk):
        if DEBUGGER.active:
            print(("{} - add_data_chunk (data_chunk={}, labels_chunk={}, "
                   + "scale_chunk={}): Start").format(
                basename(__file__), data_chunk, labels_chunk, scale_chunk))

        if scale_chunk == []:
            scale = None
        else:
            scale = scale_chunk

        self.model.add_data_chunk(data_chunk,
                                  labels_chunk,
                                  scale)

    def add_layer(self,
                  layer_outputs):
        if DEBUGGER.active:
            print("{} - add_layer (layer_outputs={}): Start".format(basename(__file__),
                                                                    layer_outputs))

        self.model.add_layer(layer_outputs)

    def get_weights(self):
        if DEBUGGER.active:
            print("{} - get_weights (): Start".format(basename(__file__)))

        return self.model.get_weights()

    def initialize(self,
                   num_inputs,
                   num_outputs,
                   learning_rate):
        if DEBUGGER.active:
            print("{} - initialize (num_inputs={}, num_outputs={}, learning_rate={}): Start".format(
                basename(__file__), num_inputs, num_outputs, learning_rate))

        self.model = Model()
        self.model.initialize(num_inputs,
                              num_outputs,
                              learning_rate)

    def predict(self,
                data):
        if DEBUGGER.active:
            print("{} - predict (data={}): Start".format(basename(__file__), data))

        return self.model.predict(data)

    def set_cost(self,
                 cost):
        if DEBUGGER.active:
            print("{} - set_cost (cost={}): Start".format(basename(__file__), cost))

        self.model.set_cost(cost)

    def set_learning_rate(self,
                          learning_rate):
        if DEBUGGER.active:
            print("{} - set_learning_rate (learning_rate={}): Start".format(basename(__file__),
                                                                            learning_rate))

        self.model.set_learning_rate(learning_rate)

    def set_weights(self,
                    new_weights):
        if DEBUGGER.active:
            print(
                "{} - set_weights (new_weights={}): Start".format(basename(__file__), new_weights))

        self.model.set_weights(new_weights)

    def train(self,
              epochs,
              batch_size):
        if DEBUGGER.active:
            print("{} - train (epochs={}, batch_size={}): Start".format(basename(__file__), epochs,
                                                                        batch_size))

        return self.model.train(epochs,
                                batch_size)


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
