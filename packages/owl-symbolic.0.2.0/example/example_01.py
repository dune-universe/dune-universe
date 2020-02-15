#!/usr/bin/env python3

### Eval option 1: onnxruntime 
import numpy as np 
import onnxruntime as rt

sess = rt.InferenceSession("test.onnx")
pred_onx = sess.run(None, {})[0]
print(pred_onx)

### Eval option 2: PyTorch (Caffe2) 
"""
"""

### Expected Output 
"""
array(666., dtype=float32)
"""