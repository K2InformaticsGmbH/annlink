# annlink
Link between eralgn applications and deep learning frameworks.

# Installation

```
git clone https://github.com/K2InformaticsGmbH/annlink.git
```

## Python

Form the ``src_py`` directory run the ``server.py`` scirpt in a python environment with access to numpy, pandas and tensorflow (Note this will block the terminal):

```
cd src_py
python3 server.py
```

## Erlang

The application can we used by itself or included in another OTP application, for developing purposes it can be run from the rebar3 shell:

```
rebar3 shell
```

## XOR Trainig

The simplest network is a XOR test, provided the server script is running locally:

```
{ok, Conn} = annlink:new_connection('127.0.0.1', 8778).
annlink:create_neural_network(Conn, [2, 10, 1]).
Inputs = [[0,0],[0,1],[1,0],[1,1]].
Labels = [[0],[1],[1],[0]].
annlink:add_data_chunk(Conn, Inputs, Labels).
annlink:set_learning_rate(Conn, 0.05).
annlink:train(Conn).
```

That will train the network for one epoch, the output should be something like:

```
1> {ok, Conn} = annlink:new_connection('127.0.0.1', 8778).
{ok,<<"A3zfatHw5jIZVsVaNYDKAemgg0qvQ+le">>}
2> annlink:create_neural_network(Conn, [2, 10, 1]).
{ok,<<"A3zfatHw5jIZVsVaNYDKAemgg0qvQ+le">>}
3> Inputs = [[0,0],[0,1],[1,0],[1,1]].
[[0,0],[0,1],[1,0],[1,1]]
4> Labels = [[0],[1],[1],[0]].
[[0],[1],[1],[0]]
5> annlink:add_data_chunk(Conn, Inputs, Labels).
ok
6> annlink:set_learning_rate(Conn, 0.05).
ok
7> annlink:train(Conn).
0.14462602138519287
``` 

``0.14...`` is the loss of the network after the training, we can train 200 epochs 5 times and see how the loss decreases:

```
[annlink:train(Conn, 200) || _ <- lists:seq(1,5)].
```

which should produce something close to:
```
[0.126319688744843,0.05803197836337134,
 1.3663458995789856e-8,6.92154666914746e-17,
 6.938893903907228e-18]
```

Finally we can use the network to make predictions:

```
annlink:predict(Conn, [[0,0], [0,1], [1,0], [1,1]]).
[[0.0],[1.0],[1.0],[0.0]]
```
