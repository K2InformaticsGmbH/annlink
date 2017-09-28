# annlink
Link between eralgn applications and deep learning frameworks.

## Installation

```
1. install the Apche Thrift compiler (see https://thrift.apache.org/ Build and Install the Apache Thrift compiler)
2. install a Python 3 environment including the packages numpy, pandas and tensorflow
3. git clone https://github.com/K2InformaticsGmbH/annlink
4. cd annlink
5. thrift_compile.bat (generates the sources from the Thrift files)  
6. rebar3 compile
7. cd ..
8. git clone https://github.com/apache/thrift
9. cd thrift/lib/py
10. python setup.py install
```

## Python

Form the ``annlink\src_py`` directory run the ``server.py`` script (Note this will block the terminal):

```
cd annlink\src_py
python server.py
```

A simple XOR test can now be run locally as follows (from directory ``annlink\src_py``):

```
python test_client.py
```

## Erlang

The application can be used by itself or included in another OTP application, for developing purposes it can be run from the rebar3 shell:

```
rebar3 shell
```

Before running the eunit tests, you must first start the Python server (from directory ``annlink\src_py``):

```
python server.py
```

Then start from the root directory (directory ``annlink``):

```
rebar3 eunit
```

## XOR Trainig

The simplest network is a XOR test, provided the server script is running locally:

```
{ok, Conn} = annlink:connect('127.0.0.1', 8778).
ok = annlink:create_neural_network(Conn, [2, 10, 1]).
Inputs = [[0,0],[0,1],[1,0],[1,1]].
Labels = [[0],[1],[1],[0]].
ok = annlink:add_data_chunk(Conn, Inputs, Labels, []).
ok = annlink:set_learning_rate(Conn, 0.05).
TrainResult = annlink:train(Conn).
TrainResults = [annlink:train(Conn, 200) || _ <- lists:seq(1, 5)].
Prediction = annlink:predict(Conn, [[0,0], [0,1], [1,0], [1,1]]).
ok = annlink:disconnect(Conn).
```

That will train the network for one epoch, the output should be something like:

```
1> {ok, Conn} = annlink:connect('127.0.0.1', 8778).
{ok,<<"/1XMXS93/dATyaSn/G+kwA6xINBMaBwL">>}
2> ok = annlink:create_neural_network(Conn, [2, 10, 1]).
ok
3> Inputs = [[0,0],[0,1],[1,0],[1,1]].
[[0,0],[0,1],[1,0],[1,1]]
4> Labels = [[0],[1],[1],[0]].
[[0],[1],[1],[0]]
5> ok = annlink:add_data_chunk(Conn, Inputs, Labels, []).
ok
6> ok = annlink:set_learning_rate(Conn, 0.05).
ok
7> TrainResult = annlink:train(Conn).
0.14417964220046997
``` 

``0.14...`` is the loss of the network after the training, we can train 200 epochs 5 times and see how the loss decreases:

```
7> TrainResults = [annlink:train(Conn, 200) || _ <- lists:seq(1, 5)].
```

which should produce something close to:
```
[0.1263104311004281,0.053840940832066406,
 5.824401515842903e-9,2.7134544611229214e-16,0.0]
```

Finally we can use the network to make predictions:

```
Prediction = annlink:predict(Conn, [[0,0], [0,1], [1,0], [1,1]]).
[[0.0],[1.0],[1.0],[0.0]]
```
