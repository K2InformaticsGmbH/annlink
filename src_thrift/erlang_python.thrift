namespace py erlang_python

typedef i64 model_id
typedef list<double> row_doubles
typedef list<row_doubles> layer
typedef list<layer> weights

service ErlangPythonServices {

   void add_activation(1:model_id model_id,
                       2:string activation),

   void add_data_chunk(1:model_id model_id,
                       2:layer data_chunk,
                       3:layer labels_chunk,
                       4:layer scale_chunk),

   void add_layer(1:model_id model_id,
                  2:i64 layer_outputs),

   weights get_weights(1:model_id model_id),

   layer predict(1:model_id model_id,
                 2:layer data),

   model_id initialize_model(1:i64 num_inputs,
                              2:i64 num_outputs,
                              3:double learning_rate),

   void set_cost(1:model_id model_id,
                 2:string cost),

   void set_learning_rate(1:model_id model_id,
                          2:double learning_rate),

   void set_weights(1:model_id model_id,
                    2:weights new_weights),

   void terminate_model(1:model_id model_id),

   double train(1:model_id model_id,
                2:i64 epochs,
                3:i64 batch_size),
}
