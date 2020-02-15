#!/usr/bin/env python3

### Eval option 1: onnxruntime 
import numpy as np 
import math
import onnxruntime as rt

sess = rt.InferenceSession("test.onnx")
# NOTE: I have to know there are two input entries and their types/shapes
input_name_x = sess.get_inputs()[0].name
input_name_y = sess.get_inputs()[1].name

# NOTE: Inputs must be of type ndarray
x = np.asarray(math.pi, dtype="float32")
y = np.asarray(3., dtype="float32")

pred_onx = sess.run(None, {input_name_x: x, input_name_y: y})[0]
print(pred_onx)

### Expected results:
# exp + 10 * y^2 = 92.71828