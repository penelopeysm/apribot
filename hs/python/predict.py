#!/usr/bin/env python3

"""
This script takes a post from standard input and prints the predicted label (0
for not Aprimon, 1 for Aprimon).

Usage:      echo 'post contents' | ./predict.py
"""

import joblib
import sys
from pathlib import Path
from utils import cleanup

# Load the model from the file
model = joblib.load(Path(__file__).parent / 'model.joblib')

# Read the post from standard input
post = " ".join(sys.stdin.readlines())

# Make a prediction
y_pred = model.predict([cleanup(post)])

# Print the prediction
print(y_pred[0])
