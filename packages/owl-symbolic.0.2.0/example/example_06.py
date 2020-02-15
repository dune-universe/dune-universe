#!/usr/bin/env python3

### Eval option 1: onnxruntime 
import numpy as np 
import onnxruntime as rt

sess = rt.InferenceSession("test.onnx")
input_name_x = sess.get_inputs()[0].name
input_name_shape = sess.get_inputs()[0].shape
input_x = np.ones(input_name_shape , dtype="float32")
pred_onx = sess.run(None, {input_name_x: input_x})[0]
#print(pred_onx)
print(pred_onx.shape)



### Expected Result

"""
Well, forget about the result, as long as there is seamingly reasonable result, it's a success.
Come back to verifying the result later.
"""