#!/usr/bin/env python3

import onnx
import torch

if __name__ == "__main__":
    model = onnx.load("test.onnx")
    onnx.checker.check_model(model)
    print(model)