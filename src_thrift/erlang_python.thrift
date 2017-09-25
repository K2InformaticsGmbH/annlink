namespace py erlang_python

typedef list<double> row_doubles
typedef list<row_doubles> layer
typedef list<layer> weights

service ErlangPythonServices {

   void add_activation(1:string activation),

   void add_data_chunk(1:layer data_chunk,
                       2:layer labels_chunk,
                       3:layer scale_chunk),

   void add_layer(1:i64 layer_outputs),

   weights get_weights(),

   layer predict(1:layer data),

   void initialize(1:i64 num_inputs,
                   2:i64 num_outputs,
                   3:double learning_rate),

   void set_cost(1:string cost),

   void set_learning_rate(1:double learning_rate),

   void set_weights(1:weights new_weights),

   double train(1:i64 epochs,
                2:i64 batch_size),
}
