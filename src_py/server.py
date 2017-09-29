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
        self.models = dict()
        self.model = None


    def add_activation(self,
                       model_id,
                       activation):
        if DEBUGGER.active:
            print(
                "{} - model {} - add_activation (activation={}): Start".format(basename(__file__),
                                                                               model_id,
                                                                               activation))

        self.model = self.models[model_id]

        self.model.add_activation(activation)

        self.models[model_id] = self.model


    def add_data_chunk(self,
                       model_id,
                       data_chunk,
                       labels_chunk,
                       scale_chunk):
        if DEBUGGER.active:
            print(("{} - model {} - add_data_chunk (data_chunk={}, labels_chunk={}, "
                   + "scale_chunk={}): Start").format(basename(__file__), model_id, data_chunk,
                                                      labels_chunk, scale_chunk))

        self.model = self.models[model_id]

        if not scale_chunk:
            scale = None
        else:
            scale = scale_chunk

        self.model.add_data_chunk(data_chunk,
                                  labels_chunk,
                                  scale)

        self.models[model_id] = self.model


    def add_layer(self,
                  model_id,
                  layer_outputs):
        if DEBUGGER.active:
            print("{} - model {} - add_layer (layer_outputs={}): Start".format(basename(__file__),
                                                                               model_id,
                                                                               layer_outputs))

        self.model = self.models[model_id]

        self.model.add_layer(layer_outputs)

        self.models[model_id] = self.model


    def get_weights(self,
                    model_id, ):
        if DEBUGGER.active:
            print("{} - model {} - get_weights (): Start".format(basename(__file__), model_id)),

        self.model = self.models[model_id]

        return self.model.get_weights()


    def initialize_model(self,
                         num_inputs,
                         num_outputs,
                         learning_rate):
        self.model = Model()
        model_id = id(self.model)

        if DEBUGGER.active:
            print(
                (
                    "{} - model {} - initialize (num_inputs={}, " +
                    "num_outputs={}, learning_rate={}): Start").format(basename(__file__),
                                                                       model_id,
                                                                       num_inputs, num_outputs,
                                                                       learning_rate))

        self.model.initialize_model(num_inputs,
                                    num_outputs,
                                    learning_rate)

        self.models[model_id] = self.model

        return model_id


    def predict(self,
                model_id,
                data):
        if DEBUGGER.active:
            print("{} - model {} - predict (data={}): Start".format(basename(__file__), model_id,
                                                                    data))

        self.model = self.models[model_id]

        prediction = self.model.predict(data)

        self.models[model_id] = self.model

        return prediction


    def set_cost(self,
                 model_id,
                 cost):
        if DEBUGGER.active:
            print("{} - model {} - set_cost (cost={}): Start".format(basename(__file__), model_id,
                                                                     cost))

        self.model = self.models[model_id]

        self.model.set_cost(cost)

        self.models[model_id] = self.model


    def set_learning_rate(self,
                          model_id,
                          learning_rate):
        if DEBUGGER.active:
            print("{} - model {} - set_learning_rate (learning_rate={}): Start".format(
                basename(__file__),
                model_id,
                learning_rate))

        self.model = self.models[model_id]

        self.model.set_learning_rate(learning_rate)

        self.models[model_id] = self.model


    def set_weights(self,
                    model_id,
                    new_weights):
        if DEBUGGER.active:
            print(
                "{} - model {} - set_weights (new_weights={}): Start".format(basename(__file__),
                                                                             model_id,
                                                                             new_weights))

        self.model = self.models[model_id]

        self.model.set_weights(new_weights)

        self.models[model_id] = self.model


    def terminate_model(self,
                        model_id):
        if DEBUGGER.active:
            print(
                "{} - model {} - terminate_model (): Start".format(basename(__file__), model_id))

        del self.models[model_id]

        if DEBUGGER.active:
            print(
                "{} - model {} - terminate_model (): active models {}".format(basename(__file__),
                                                                              model_id,
                                                                              len(self.models)))


    def train(self,
              model_id,
              epochs,
              batch_size):
        if DEBUGGER.active:
            print("{} - model {} - train (epochs={}, batch_size={}): Start".format(
                basename(__file__), model_id, epochs, batch_size))

        self.model = self.models[model_id]

        result = self.model.train(epochs,
                                  batch_size)

        self.models[model_id] = self.model

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
