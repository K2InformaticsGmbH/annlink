import configparser
from os.path import basename

from erlang_python import ErlangPythonServices
from thrift import Thrift
from thrift.protocol import TBinaryProtocol
from thrift.transport import TSocket
from thrift.transport import TTransport


class Dbg(object):
    active = False


DEBUGGER = Dbg()


def main():
    if DEBUGGER.active:
        print("{} - main (): Start".format(basename(__file__)))

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

    print(("{} - main (): This client will connect to a server with " +
           "ip address {} and port number {}").format(basename(__file__), host, port))

    # --------------------------------------------------------------------------
    # Init thrift connection and protocol handlers
    # --------------------------------------------------------------------------

    # Make socket
    transport = TSocket.TSocket(host, port)

    # Buffering is critical. Raw sockets are very slow
    transport = TTransport.TBufferedTransport(transport)

    # Wrap in a protocol
    protocol = TBinaryProtocol.TBinaryProtocol(transport)

    # Create a client to use the protocol encoder
    client = ErlangPythonServices.Client(protocol)

    # Connect to server
    transport.open()

    # --------------------------------------------------------------------------
    # XOR Training
    # --------------------------------------------------------------------------

    # --------------------------------------------------------------------------
    # > annlink:create_neural_network(Conn, [2, 10, 1]).
    # {ok,<<"A3zfatHw5jIZVsVaNYDKAemgg0qvQ+le">>}
    # --------------------------------------------------------------------------

    # create_neural_network(NetworkId, [InputSize | Rest], Activation) when is_atom(Activation) ->
    #     ok = initialize(NetworkId, InputSize, lists:last(Rest)),
    #     ok = add_layers(NetworkId, Rest, Activation),

    # initialize(NetworkId, InpSize, OutSize) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {initialize, InpSize, OutSize}).

    num_inputs = 2
    num_outputs = 1
    learning_rate = 0.001
    client.initialize(id(client),
                      num_inputs,
                      num_outputs,
                      learning_rate)

    # add_layers(NetworkId, [Size], _Activation) ->
    #     add_layer(NetworkId, Size);
    # add_layers(NetworkId, [Size | Rest], Activation) ->
    #     add_layer(NetworkId, Size),
    #     add_activation(NetworkId, Activation),
    #     add_layers(NetworkId, Rest, Activation).

    # add_layer(NetworkId, Size) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {add_layer, Size}).

    size = 10
    client.add_layer(id(client),
                     size)

    # add_activation(NetworkId, Activation) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {add_activation, Activation}).

    activation = "sigmoid"
    client.add_activation(id(client),
                          activation)

    # add_layer(NetworkId, Size) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {add_layer, Size}).

    size = 1
    client.add_layer(id(client),
                     size)

    # --------------------------------------------------------------------------
    # > Inputs = [[0,0],[0,1],[1,0],[1,1]].
    # [[0,0],[0,1],[1,0],[1,1]]
    # > Labels = [[0],[1],[1],[0]].
    # [[0],[1],[1],[0]]
    # > annlink:add_data_chunk(Conn, Inputs, Labels).
    # ok
    # --------------------------------------------------------------------------

    # add_data_chunk(NetworkId, Inputs, Labels, Scale) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {add_data_chunk, [Inputs, Labels, Scale]}).

    inputs = [[0, 0], [0, 1], [1, 0], [1, 1]]
    labels = [[0], [1], [1], [0]]
    scale = []
    client.add_data_chunk(id(client),
                          inputs,
                          labels,
                          scale)

    # --------------------------------------------------------------------------
    # > annlink:set_learning_rate(Conn, 0.05).
    # ok
    # --------------------------------------------------------------------------

    # set_learning_rate(NetworkId, LearningRate) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {set_learning_rate, LearningRate}).

    learning_rate = 0.05
    client.set_learning_rate(id(client),
                             learning_rate)

    # --------------------------------------------------------------------------
    # > annlink:train(Conn).
    # 0.14462602138519287
    # --------------------------------------------------------------------------

    # train(NetworkId, Args) when is_list(Args) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {train, Args}).

    epochs = 1
    batch_size = 512
    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    # --------------------------------------------------------------------------
    # >[annlink:train(Conn, 200) || _ <- lists:seq(1,5)].
    # which should produce something close to:
    #
    # [0.126319688744843,0.05803197836337134,
    #  1.3663458995789856e-8,6.92154666914746e-17,
    #  6.938893903907228e-18]
    # --------------------------------------------------------------------------

    epochs = 200
    batch_size = 512
    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    result = client.train(id(client),
                          epochs,
                          batch_size)

    if DEBUGGER.active:
        print("{} - main ({}): result from train".format(basename(__file__), result))

    # --------------------------------------------------------------------------
    # >annlink:predict(Conn, [[0,0], [0,1], [1,0], [1,1]]).
    # [[0.0],[1.0],[1.0],[0.0]]
    # --------------------------------------------------------------------------

    # predict(NetworkId, [N|_]=Input) when is_number(N) ->
    #     [Result] = gen_server:call(?NETWORK_GID(NetworkId), {predict, [Input]}),
    # predict(NetworkId, Set) ->
    #     gen_server:call(?NETWORK_GID(NetworkId), {predict, Set}).

    data = [[0, 0], [0, 1], [1, 0], [1, 1]]
    result = client.predict(id(client),
                            data)

    if DEBUGGER.active:
        print("{} - main ({}): result from predict".format(basename(__file__), result))

    client.terminate_model(id(client)),

    # --------------------------------------------------------------------------
    # Terminate client
    # --------------------------------------------------------------------------

    # Close the connection
    transport.close()

    if DEBUGGER.active:
        print("{} - main (): Done".format(basename(__file__)))


if __name__ == "__main__":
    if DEBUGGER.active:
        print("{} - __main__ (): Start".format(basename(__file__)))

    try:
        main()

        if DEBUGGER.active:
            print("{} - __main__ (): Done".format(basename(__file__)))

    except Thrift.TException as tx:
        print("{} - __main__ (): Exception: {}".format(basename(__file__), tx.message))
