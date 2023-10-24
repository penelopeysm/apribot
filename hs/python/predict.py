#!/usr/bin/env python3

"""
This script takes a post from standard input and prints the predicted label.

  -1 : not Aprimon-related and predict_proba is below 0.015, meaning that the
       post is negative with high certainty. These posts are not sent to the
       website for review.
  0  : not Aprimon-related but predict_proba is above 0.015, meaning that the
       post is negative with low certainty. These posts are sent to the website
       for review (as there may be a very small chance of a false negative).
  1  : Aprimon-related. All of these posts are sent to the website for review
       (to ensure that there are no false positives).

Usage:      echo 'post contents' | ./predict.py
"""

import joblib
import sys
from pathlib import Path
from utils import cleanup

THRESHOLD_NEG = 0.015

# Load the model from the file
model = joblib.load(Path(__file__).parent / 'model.joblib')

# Read the post from standard input
post = " ".join(sys.stdin.readlines())
cleaned_post = cleanup(post)

# Get the probability of a positive post
y_pred_proba = model.predict_proba([cleaned_post])[0, 1]

# Print the output
if y_pred_proba < THRESHOLD_NEG:
    print(-1)
elif y_pred_proba < 0.5:
        print(0)
else:
    print(1)
