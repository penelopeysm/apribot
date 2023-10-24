# This script is used to train the model and save it to disk.
# For full details of model selection & cross-validation, see the notebooks in
# the separate repository: https://github.com/penelopeysm/apriml

import pandas as pd
import numpy as np
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import StackingClassifier
from xgboost import XGBClassifier
from sklearn.model_selection import train_test_split, cross_validate
from sklearn.metrics import f1_score
import joblib

from pathlib import Path

from utils import cleanup

# Columns in this dataframe:
# 
#     id:        Reddit post ID on reddit. The post itself can be viewed at
#                https://reddit.com/r/pokemontrades/comments/{id}
#     title:     Post title.
#     body:      Post body (in Markdown format).
#     submitter: The username of the submitter
#     time:      Post submission time, in UTC
#     flair:     Post flair (at the time the post was scraped)
#     hit:       Whether ApriBot's crude detection mechanism (keyword-search) thought the post was Aprimon-related.
#     vote:      /r/BankBallExchange's judgment as to whether the post was Aprimon-related. This is the ground truth.
df = pd.read_parquet('posts.parquet')
print(f'Loaded {len(df)} posts.')

df_voted_processed = (df.assign(title=df.title.apply(cleanup))
                      .assign(body=df.body.apply(cleanup))
                      .assign(post=lambda df: df.title + ' ' + df.body)
                      .loc[df['vote'].notnull(), ['id', 'title', 'body', 'post', 'vote']])

X = df_voted_processed['post'].to_numpy()
y = df_voted_processed['vote'].to_numpy().astype(int)
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=468)

print(f"1's in training set : {np.count_nonzero(y_train)} out of {len(y_train)} ({np.count_nonzero(y_train) / len(y_train):.2%})") # type: ignore
print(f"1's in test set     : {np.count_nonzero(y_test)} out of {len(y_test)} ({np.count_nonzero(y_test) / len(y_test):.2%})") # type: ignore

# Define model. The hyperparameters were optimised in the notebook.
pipeline = StackingClassifier(
    estimators=[
        ('xg', Pipeline([
            ('vec', CountVectorizer(ngram_range=(1, 2), max_features=10000)),
            ('clf', XGBClassifier(max_depth=9, n_estimators=100, objective='binary:logistic')),
        ])),
        ('lr', Pipeline([
            ('vec', CountVectorizer(ngram_range=(1, 2), max_features=10000)),
            ('clf', LogisticRegression(max_iter=5000)),
        ]))
    ],
    final_estimator=LogisticRegression(max_iter=5000)
)

print('Performing cross-validation...')

cv = cross_validate(pipeline, X_train, y_train, cv=5, scoring='f1')
print(f'CV F1 score: {np.mean(cv["test_score"]):.4%}')

print('Training...')

pipeline.fit(X_train, y_train)
y_pred = pipeline.predict(X_test)
print(f'Test F1 score: {f1_score(y_pred=y_pred, y_true=y_test):.4%}')

print('Saving model...')
joblib.dump(pipeline, Path(__file__).parent / 'model.joblib')
print('Done.')
