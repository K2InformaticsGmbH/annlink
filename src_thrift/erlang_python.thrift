namespace py erlang_python

typedef i64 client_id
typedef list<double> row_doubles
typedef list<row_doubles> layer
typedef list<layer> weights

service ErlangPythonServices {

   void add_activation(1:client_id client_id,
                       2:string activation),

   void add_data_chunk(1:client_id client_id,
                       2:layer data_chunk,
                       3:layer labels_chunk,
                       4:layer scale_chunk),

   void add_layer(1:client_id client_id,
                  2:i64 layer_outputs),

   weights get_weights(1:client_id client_id),

   layer predict(1:client_id client_id,
                 2:layer data),

   void initialize(1:client_id client_id,
                   2:i64 num_inputs,
                   3:i64 num_outputs,
                   4:double learning_rate),

   void set_cost(1:client_id client_id,
                 2:string cost),

   void set_learning_rate(1:client_id client_id,
                          2:double learning_rate),

   void set_weights(1:client_id client_id,
                    2:weights new_weights),

   void terminate_model(1:client_id client_id),

   double train(1:client_id client_id,
                2:i64 epochs,
                3:i64 batch_size),
}
