import numpy as np
import tensorflow as tf

import helper


class Model(object):
    def __init__(self):
        self.x = None
        self.y = None
        self.z = None
        self.lr = 0.001
        self.weights = []
        self.biases = []
        self.logits = None
        self.cost = None
        self.optimizer = None
        self.data = []
        self.labels = []
        self.scale = []

        self._scaled = False
        self._sess = None


    def initialize(self, num_inputs, num_outputs, learning_rate=0.001):
        tf.reset_default_graph()
        self.lr = learning_rate
        self.x = tf.placeholder(tf.float32, [None, num_inputs], 'x')
        self.logits = self.x
        self.y = tf.placeholder(tf.float32, [None, num_outputs], 'y')
        return self.x, self.y


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
            stable_tanh = 1.7159 * tf.tanh(0.6666666666666666 * self.logits)
            # Also adding a factor to avoid saturation
            self.logits = tf.add(stable_tanh, 0.001 * self.logits)
        else:
            raise ValueError('Unsupported activation function')
        return self.logits


    def set_cost(self, cost):
        if cost == 'scaled':
            num_inputs = self.logits.get_shape().as_list()[1]
            self.z = tf.placeholder(tf.float32, [None, num_inputs], 'z')
            self.cost = tf.nn.l2_loss(tf.multiply(self.logits - self.y, self.z))
            self._scaled = True
        else:
            raise ValueError('Unsupported cost function')
        return self.cost


    def add_data_chunk(self, data_chunk, labels_chunk, scale_chunk=None):
        if self._scaled:
            if scale_chunk is None:
                raise ValueError('Scale factor required for scaled cost')
            else:
                self.scale += scale_chunk

        self.data += data_chunk
        self.labels += labels_chunk
        if self._scaled:
            return self.data, self.labels, self.scale
        else:
            return self.data, self.labels


    def set_learning_rate(self, learning_rate):
        self.lr = learning_rate
        return self.lr


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


    def train(self, epochs=1, batch_size=512):
        if self._scaled:
            return self._train_scaled(epochs, batch_size)
        else:
            return self._train(epochs, batch_size)


    def _train(self, epochs, batch_size):
        loss = 0.0
        samples_count = 0
        for _ in range(epochs):
            for batch_features, batch_labels in helper.batch_features_labels(self.data, self.labels,
                                                                             batch_size):
                samples_count += len(batch_features)
                self.session.run(self.optimizer,
                                 feed_dict={self.x: batch_features, self.y: batch_labels})
                loss += self.session.run(self.cost,
                                         feed_dict={self.x: batch_features, self.y: batch_labels})
        return loss / samples_count


    def _train_scaled(self, epochs, batch_size):
        loss = 0.0
        samples_count = 0
        for _ in range(epochs):
            for features, labels, scale in helper.batch_features_labels_scale(self.data,
                                                                              self.labels,
                                                                              self.scale,
                                                                              batch_size):
                samples_count += len(features)
                self.session.run(self.optimizer,
                                 feed_dict={self.x: features, self.y: labels, self.z: scale})
                loss += self.session.run(self.cost, feed_dict={self.x: features, self.y: labels,
                                                               self.z: scale})
        return loss / samples_count


    def get_weights(self):
        if self._sess is None:
            raise ValueError("The model is not trained yet")
        result = []
        for i in range(len(self.weights)):
            layer_w = self.session.run(self.weights[i])
            layer_b = self.session.run(self.biases[i])
            # Insert bias into the layers to match ann structure.
            layer_result = np.insert(layer_w, 0, layer_b, axis=0).T
            result.append(layer_result.tolist())
        return result


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
