import numpy as np
import pandas as pd
import tensorflow as tf

class Model(object):

    def __init__(self):
        self.x = None
        self.y = None
        self.weights = []
        self.biases = []
        self.logits = None
        self.cost = None
        self.optimizer = None

        self._sess = None

    def initialize(self, num_inputs, num_outputs, learning_rate=0.001):
        tf.reset_default_graph()
        self.lr = learning_rate
        self.x = tf.placeholder(tf.float32, [None, num_inputs], 'x')
        self.logits = self.x
        self.y = tf.placeholder(tf.float32, [None, num_outputs], 'y')
        return self.x, self.y

    @property
    def session(self):
        if self.cost is None:
            self.cost = tf.nn.l2_loss(self.logits - self.y)
        if self.optimizer is None:
            self.optimizer = tf.train.AdamOptimizer(learning_rate=self.lr).minimize(self.cost)
        if self._sess is None:
            self._sess = tf.Session()
            self._sess.run(tf.global_variables_initializer())
        return self._sess

    def add_layer(self, layer_outputs):
        layer_inputs = self.logits.get_shape().as_list()[1]
        layer_weights = tf.Variable(tf.truncated_normal([layer_inputs, layer_outputs], 0.0, 0.001))
        layer_bias = tf.Variable(tf.truncated_normal([layer_outputs], 0.0, 0.001))
        self.logits = tf.add(tf.matmul(self.logits, layer_weights), layer_bias)
        self.weights.append(layer_weights)
        self.biases.append(layer_bias)
        return self.logits, layer_weights, layer_bias

    def add_activation(self, activation):
        if activation == 'relu':
            self.logits = tf.nn.relu(self.logits)
        elif activation == 'sigmoid':
            self.logits = tf.sigmoid(self.logits)
        elif activation == 'softsign':
            self.logits = tf.nn.softsign(self.logits)
        elif activation == 'tanh':
            self.logits = tf.tanh(self.logits)
        elif activation == 'tanh1':
            # From lecun paper centered and more stable tanh
            stable_tanh = 1.7159*tf.tanh(0.6666666666666666 * self.logits)
            # Also adding a factor to avoid saturation
            self.logits = tf.add(stable_tanh, 0.001 * self.logits)
        else:
            raise ValueError('Unsupported activation function')
        return self.logits

    def set_weights(self, new_weights):
        n_layers = len(self.weights)
        if len(new_weights) != n_layers:
            raise ValueError('Weights provided with different network structure')
        for i in range(n_layers):
            layer_result = np.array(new_weights[i]).T
            layer_b = layer_result[0]
            layer_w = np.delete(layer_result, 0, axis=0)
            self.biases[i].load(layer_b, self.session)
            self.weights[i].load(layer_w, self.session)

    def predict(self, data):
        return self.session.run(self.logits, feed_dict={self.x: data}).tolist()
