#!/usr/bin/env python3

### Eval option 1: onnxruntime 
import numpy as np 
import onnxruntime as rt

sess = rt.InferenceSession("test.onnx")
input_name = sess.get_inputs()[0].name
img = np.ones((3, 3)).astype(np.float32)
pred_onx = sess.run(None, {input_name: img})[0]
print(pred_onx)

### Expected output:
"""
[[667. 667. 667.]
 [667. 667. 667.]
 [667. 667. 667.]]
"""