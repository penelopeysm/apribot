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
from sklearn.model_selection import train_test_split
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

X = np.array([cleanup(t) for t in (df['title'] + ' ' + df['body']).to_list()])
y = df['vote'].to_numpy()
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=468)

# Define model
# TODO: Why does this give different results compared to the version in the
# notebook (where the featuriser is included in the stacked classifier)?
# Anyway, this outperforms the notebook version in terms of CV F1 score, so...
pipeline = Pipeline([
    ('vectorise', CountVectorizer(ngram_range=(1, 2), max_features=5000)),
    ('model', StackingClassifier(estimators=[
        ('xgb', XGBClassifier(max_depth=7, n_estimators=100,
                              objective='binary:logistic')),
        ('lr', LogisticRegression(max_iter=5000)),
    ], final_estimator=LogisticRegression(max_iter=5000)))
])

# Version which reproduces the notebook CV results perfectly
# pipeline = StackingClassifier(estimators=[
#         ('xgb', Pipeline([
#             ('vectorise', CountVectorizer(ngram_range=(1, 2), max_features=5000)),
#             ('clf', XGBClassifier(max_depth=7, n_estimators=100,
#                               objective='binary:logistic'))])),
#         ('lr', Pipeline([
#             ('vectorise', CountVectorizer(ngram_range=(1, 2), max_features=5000)),
#             ('clf', LogisticRegression(max_iter=5000))])),
#     ], final_estimator=LogisticRegression(max_iter=5000)
# )

print('Cross-validating...')
from sklearn.model_selection import cross_val_score
scores = cross_val_score(pipeline, X_train, y_train, cv=5, scoring='f1')
print(f'CV F1 score: {scores.mean():.4%}')

print('Training...', end=' ')
pipeline.fit(X_train, y_train)
y_pred = pipeline.predict(X_test)
print(f'Test F1 score: {f1_score(y_pred=y_pred, y_true=y_test):.4%}')

print('Saving model...')
joblib.dump(pipeline, Path(__file__).parent / 'model.joblib')
print('Done.')
