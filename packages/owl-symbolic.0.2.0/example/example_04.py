#!/usr/bin/env python3

### Eval option 1: onnxruntime 
import numpy as np 
import onnxruntime as rt

sess = rt.InferenceSession("test.onnx")

# Note how the initializer works without usr providing input 
pred_onx = sess.run(None, input_feed={})
print(pred_onx[0])

# The user can also provide her own input 
input_x = sess.get_inputs() # NOTE: This gives an empty set this time. 

input_x = sess.get_overridable_initializers()[0]
input_name_x = input_x.name 
input_shape_x = input_x.shape

# Check input_x.type, we find the type is tensor(float)
x = np.ones(input_shape_x, dtype="float32")

# NOTE: x = np.ones(input_shape_x, dtype="float") will leads to an error

pred_onx = sess.run(None, {input_name_x: x})
print(pred_onx[0])

### Expected output
"""
[[ 0.84147096  0.9092974   0.14112   ]
 [-0.7568025  -0.9589243  -0.2794155 ]]
[[0.84147096 0.84147096 0.84147096]
 [0.84147096 0.84147096 0.84147096]]
"""
