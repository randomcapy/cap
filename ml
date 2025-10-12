
1	Practical 1 
Implementing the KNN algorithm
(To classify handwritten digits)	
8th Sep 2025	
2	Practical 2 
Building a decision tree model using the ID3 algorithm
(To predict whether a customer will churn or not)	
16th Sep 2025	
3	Practical 3 
Developing a support vector machine (SVM) model
(To classify email messages as spam or not spam)	
8th Sep 2025	
4	Practical 4 
Building a Naïve Bayes classifier
(To classify movie reviews as positive or negative sentiments)	
23th Sep 2025	
5	Practical 5 
Implementing linear regression
(To predict housing prices based on features such as size and location)	
1th Sep 2025	
6	Practical 6 
Using logistic regression
(To predict whether a credit card transaction is fraudulent or not)	
2th Sep 2025	
7	Practical 7 
Evaluating a classification model using metrices such as accuracy, Precision, recall, and F1-Score	
1th Sep 2025	
8	Practical 8 
Applying hierarchical clustering
(To group customer segments based on their purchasing behaviour)	
6th Oct 2025	
9	Practical 9 
Implementing the K-means clustering algorithm
(To identify the distinct clusters in a customer demographics dataset)	
8th Sep 2025	
10	Practical 10 
Utilizing principal component analysis (PCA) for dimensionality reduction to improve the efficiency and interpretability of a model	
6th Oct 2025	


 
Practical No.1
Implementing the KNN algorithm (To classify handwritten digits)

Code: -
# Importing Libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import classification_report, accuracy_score, confusion_matrix
Output: -
 

Code: -
# Load the digits dataset
digits = datasets.load_digits()
X = digits.data      # Feature matrix (images flattened)
y = digits.target    # Labels (digit 0-9)
print("Dataset shape:", X.shape)
 
Output: -
 

Code: -
# Split into train/test sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
# Feature scaling (important for KNN)
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)
Output: -
 

Code: -
# Initialize KNN with k=3
knn = KNeighborsClassifier(n_neighbors=3)
knn.fit(X_train, y_train)
Output: - 
 
Code: -
# Predict on test data
y_pred = knn.predict(X_test)
# Evaluation
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))
Output: -
 

Code: -
# Confusion matrix visualization
plt.figure(figsize=(6,4))
plt.imshow(confusion_matrix(y_test, y_pred), cmap='Blues', interpolation='nearest')
plt.title("Confusion Matrix")
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.colorbar()
plt.show()
 
Output: -
 

Code: -
# Show a few test images with predictions
for i in range(5):
    plt.figure(figsize=(6,4))
    plt.imshow(X_test[i].reshape(8,8), cmap='gray')
    plt.title(f"True: {y_test[i]}, Pred: {y_pred[i]}")
    plt.show()
Output: -









 
Practical No.2
Building a decision tree model using the ID3 algorithm
(To predict whether a customer will churn or not)

Code: -
# Import Libraries
import pandas as pd 
from sklearn.model_selection import train_test_split 
from sklearn.tree import DecisionTreeClassifier, plot_tree  
from sklearn.metrics import accuracy_score, classification_report  
import matplotlib.pyplot as plt
Output: -
 

Code: -
# Load the Dataset
df= pd.read_csv("/content/drive/MyDrive/ML Data/Decision Tree/WA_Fn-UseC_-Telco-Customer-Churn.csv")
print(f"Shape:",df.shape)
print(df.head())
 
Output: -
 

Code: -
# Preprocess the Data
# Drop customerID
df = df.drop('customerID', axis=1, errors="ignore")
# Convert 'TotalCharges' to numeric (coerce errors to NaN)
df['TotalCharges'] = pd.to_numeric(df['TotalCharges'], errors='coerce')
# Fill missing values if any
df['TotalCharges'] = df['TotalCharges'].fillna(df['TotalCharges'].median())
# Convert target variable
df['Churn'] = df['Churn'].map({'No': 0, 'Yes': 1})
# Convert categorical variables to dummy/indicator variables
df = pd.get_dummies(df, drop_first=True)
print(df.columns)
 
Output: -
 

Code: -
# Split the Data
X = df.drop('Churn', axis=1)
y = df['Churn']
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42)
Output: -
 
Code: -
# Building and Train the Decision Tree Model
dtree = DecisionTreeClassifier(criterion='entropy', max_depth=5, random_state=42)
dtree.fit(X_train, y_train)
Output: -
 

Code: -
# Make prediction
y_pred = dtree.predict(X_test)
print(y_pred)
Output: -
 

Code: -
# Evaluate the Model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))
 
Output: -
 

Code: -
# Visualize the Tree
plt.figure(figsize=(20,10))
plot_tree( dtree, feature_names=X.columns, class_names=['No Churn', 'Churn'],
filled=True, max_depth=2 # Only show top 2 levels for readability)
plt.title("Decision Tree (ID3) - Top 2 Levels")
plt.show()
Output: -







  
Practical No.3
Developing a support vector machine (SVM) model
(To classify email messages as spam or not spam)

Code: -
# Import Libraries 
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.svm import SVC
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix

# Step 1: Load the dataset (tab-separated, no header)
df = pd.read_csv('/content/drive/MyDrive/ML Data/ML Journal datasets/Practical No.3/SMSSpamCollection.csv', sep='\t', header=None, names=['label', 'message'])

# Step 2: Encode labels (ham=0, spam=1)
df['label'] = df['label'].map({'ham': 0, 'spam': 1})

# Step 3: Split into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(
    df['message'], df['label'], test_size=0.3, random_state=42, stratify=df['label'])

# Step 4: Convert text messages to TF-IDF features
vectorizer = TfidfVectorizer(stop_words='english')
X_train_vec = vectorizer.fit_transform(X_train)
X_test_vec = vectorizer.transform(X_test)

# Step 5: Train the SVM classifier (linear kernel)
svm = SVC(kernel='linear', random_state=42)
svm.fit(X_train_vec, y_train)

# Step 6: Make predictions
y_pred = svm.predict(X_test_vec)

# Step 7: Evaluate the model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))
print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred))
Output: -
 







 
Practical No.4
Building a Naïve Bayes classifier
(To classify movie reviews as positive or negative sentiments)

Code: -
pip install nltk scikit-learn
Output: -
 

Code: -
import nltk
import random
from nltk.corpus import movie_reviews
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import accuracy_score, classification_report
Output: -
 

Code: -
import nltk
nltk.download('movie_reviews')
 
Output: -
 

Code: -
# Load movie review file IDs and their categories (pos/neg)
documents = [(movie_reviews.raw(fileid), category)
             for category in movie_reviews.categories()
             for fileid in movie_reviews.fileids(category)]

# Shuffle the documents for randomness
random.shuffle(documents)

# Separate reviews and their labels
texts = [review for (review, label) in documents]
labels = [1 if label == 'pos' else 0 for (review, label) in documents]  # 1=positive, 0=negative
Output: -
 

Code: -
X_train, X_test, y_train, y_test = train_test_split(
    texts, labels, test_size=0.3, random_state=42)
Output: -
 
Code: -
vectorizer = CountVectorizer(stop_words='english')
X_train_vec = vectorizer.fit_transform(X_train)
X_test_vec = vectorizer.transform(X_test)
Output: -
 

Code: -
nb = MultinomialNB()
nb.fit(X_train_vec, y_train)
Output: -
 

Code: -
y_pred = nb.predict(X_test_vec)
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred, target_names=['Negative', 'Positive']))
Output: -









  
Practical No.5
Implementing linear regression
(To predict housing prices based on features such as size and location)

Code: -
# Import Libraries 
import pandas as pd
import numpy as np
from sklearn.datasets import fetch_california_housing
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt
Output: -
 

Code: -
# Load the Dataset
# Load California housing dataset
housing = fetch_california_housing(as_frame=True)
df = housing.frame
print(df.head())
 
Output: -
 

Code: -
# Split Data into Features and Target
X = df.drop('MedHouseVal', axis=1)  # Features
y = df['MedHouseVal']               # Target variable (median house value)
Output: -
 

Code: -
# Train-Test Split
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42)
Output: -
 
 
Code: -
# Build and Train the Linear Regression Model
lr = LinearRegression()
lr.fit(X_train, y_train)
Output: -
 

Code: -
# Make Predictions
y_pred = lr.predict(X_test)
Output: -
 

Code: -
# Evaluate the Model
print("Mean Squared Error:", mean_squared_error(y_test, y_pred))
print("R2 Score:", r2_score(y_test, y_pred))
Output: -
 
 
Code: -
# Visualize Predictions vs. Actual
plt.scatter(y_test, y_pred, alpha=0.5)
plt.xlabel('Actual Median House Value')
plt.ylabel('Predicted Median House Value')
plt.title('Actual vs Predicted House Values')
plt.show()
Output: -









 
Practical No.6
Using logistic regression
(To predict whether a credit card transaction is fraudulent or not)

Code: -
# Import Libraries
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score
Output: -
 

Code: -
# Load the Dataset
df = pd.read_csv('/content/drive/MyDrive/ML Data/ML Journal datasets/Practical No.6/creditcard.csv')
print(df.head())
 
Output: -
 

Code: -
# Prepare Data
X = df.drop('Class', axis=1)
y = df['Class']
scaler = StandardScaler()
X[['Time', 'Amount']] = scaler.fit_transform(X[['Time', 'Amount']])
Output: -
 

Code: -
# Train-Test Split
X_train, X_test, y_train, y_test = train_test_split (X, y, test_size=0.3, random_state=42, stratify=y)
 
Output: -
 

Code: -
# Build and Train Logistic Regression Model
lr = LogisticRegression(max_iter=1000)
lr.fit(X_train, y_train)
Output: -
 

Code: -
# Make Predictions
y_pred = lr.predict(X_test)
Output: -
 

Code: -
# Evaluate the Model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))

 
Output: -









Practical No.7
Evaluating a classification model using metrices such as accuracy, Precision, recall, and F1-Score

Code: -
# Import Libraries
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, classification_report
Output: -
 

Code: -
# Load Dataset and Split
# Load the Iris dataset (for demo; replace with your own as needed)
iris = load_iris()
X = iris.data
y = iris.target

# For binary classification (let's predict if species is "setosa" or not)
import numpy as np
y_binary = np.where(y == 0, 1, 0)  # 1 = setosa, 0 = not setosa

# Split dataset
X_train, X_test, y_train, y_test = train_test_split(X, y_binary, test_size=0.3, random_state=42)
 
Output: -
 

Code: -
# Train a Classification Model
model = LogisticRegression()
model.fit(X_train, y_train)
y_pred = model.predict(X_test)
Output: -
 

Code: -
# Evaluate Model Using Metrics

# Accuracy
accuracy = accuracy_score(y_test, y_pred)
# Precision
precision = precision_score(y_test, y_pred)
# Recall
recall = recall_score(y_test, y_pred)
# F1-Score
f1 = f1_score(y_test, y_pred)


# Print the results
print("Accuracy:", accuracy)
print("Precision:", precision)
print("Recall:", recall)
print("F1-Score:", f1)

# Or use classification_report for a summary
print("\nClassification Report:\n", classification_report(y_test, y_pred, target_names=['Not Setosa', 'Setosa']))
Output: -








  
Practical No.8
Applying hierarchical clustering
(To group customer segments based on their purchasing behavior)

Code: -
# Import Libraries
import pandas as pd
import numpy as np
from scipy.cluster.hierarchy import linkage, dendrogram, fcluster
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
Output: -
 

Code: -
# Load the dataset
df = pd.read_csv('/content/drive/MyDrive/ML Data/ML Journal datasets/Practical No.8/Mall_Customers.csv')
print(df.head())
Output: -
 
 
Code: -
# Select features for clustering (e.g., Annual Income and Spending Score)
X = df[['Annual Income (k$)', 'Spending Score (1-100)']]
# Standardize the features
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)
Output: -
 

Code: -
# Perform hierarchical clustering
Z = linkage(X_scaled, method='ward')
Output: -
 

Code: -
#  Visualize dendrogram
plt.figure(figsize=(12, 6))
dendrogram(Z)
plt.title('Dendrogram for Mall Customers')
plt.xlabel('Customer Index')
plt.ylabel('Distance')
plt.show()
 
Output: -
 
 

Code: -
# Choose number of clusters (e.g., 5) and assign cluster labels
num_clusters = 5
df['Cluster'] = fcluster(Z, num_clusters, criterion='maxclust')

# Visualize clusters
plt.figure(figsize=(8, 6))
for cluster in range(1, num_clusters+1):
    plt.scatter( df[df['Cluster'] == cluster]['Annual Income (k$)'],
    df[df['Cluster'] == cluster]['Spending Score (1-100)'], label=f'Cluster {cluster}' )
plt.xlabel('Annual Income (k$)'), plt.ylabel('Spending Score (1-100)')
plt.title('Customer Segments by Hierarchical Clustering')
plt.legend()
plt.show()
Output: -
 
 

 
Code: -
# Review cluster assignments
print(df[['CustomerID', 'Cluster']].head())
Output: -
 










Practical No.9
Implementing the K-means clustering algorithm
(To identify the distinct clusters in a customer demographics dataset)

Code: -
# Import Libraries
import pandas as pd
import numpy as np
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
Output: -
 

Code: -
# Load the Dataset
# Load the dataset (ensure 'Mall_Customers.csv' is in your working directory)
df = pd.read_csv('/content/drive/MyDrive/ML Data/ML Journal datasets/Practical No.8/Mall_Customers.csv')
print(df.head())
 
Output: -
 

Code: -
# Select Features
X = df[['Age', 'Annual Income (k$)', 'Spending Score (1-100)']]
Output: -
 

Code: -
# Feature Scaling
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)
Output: -
 

Code: -
# Find Optimal Number of Clusters (Elbow Method)
wcss = []  # Within-cluster sum of squares
for i in range(1, 11):
    kmeans = KMeans(n_clusters=i, random_state=42)
    kmeans.fit(X_scaled)
    wcss.append(kmeans.inertia_)

plt.plot(range(1, 11), wcss, marker='o')
plt.title('Elbow Method - Optimal k')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()
Output: -
  
 
Code: -
# Apply K-means with Optimal k
kmeans = KMeans(n_clusters=5, random_state=42)
clusters = kmeans.fit_predict(X_scaled)
df['Cluster'] = clusters
Output: -
 

Code: -
# Visualize the Clusters
plt.figure(figsize=(8,6))
for cluster in range(5):
    plt.scatter(
        df[df['Cluster'] == cluster]['Annual Income (k$)'],
        df[df['Cluster'] == cluster]['Spending Score (1-100)'],
        label=f'Cluster {cluster}' )
plt.xlabel('Annual Income (k$)')
plt.ylabel('Spending Score (1-100)')
plt.title('Customer Segments (K-means Clustering)')
plt.legend()
plt.show()
 
Output: -
 
 


 
Code: -
# Review Cluster Assignments
print(df[['CustomerID', 'Age', 'Annual Income (k$)', 'Spending Score (1-100)', 'Cluster']].head())
Output: -










Practical No.10
Utilizing principal component analysis (PCA) for dimensionality reduction to improve the efficiency and interpretability of a model

Code: -
# Import Libraries
import pandas as pd
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
Output: -
 

Code: -
# Load and Prepare the Dataset
# Load the dataset
iris = load_iris()
X = iris.data
y = iris.target

 
# Standardize features (important for PCA)
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)
Output: -
 

Code: -
# Apply PCA for Dimensionality Reduction
pca = PCA(n_components=2)
X_pca = pca.fit_transform(X_scaled)
# Show how much variance is explained by each component
print("Explained variance ratio:", pca.explained_variance_ratio_)
print("Total variance explained:", sum(pca.explained_variance_ratio_))
Output: -
 

Code: -
# Visualize the Reduced Data
plt.figure(figsize=(8,6))
plt.scatter(X_pca[:, 0], X_pca[:, 1], c=y, cmap='viridis', edgecolor='k')
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.title('PCA of Iris Dataset')
plt.show()
Output: -
 
 

 
Code: -
# Train a Model Using the Reduced Data
# Split the PCA-transformed data
X_train, X_test, y_train, y_test = train_test_split(X_pca, y, test_size=0.3, random_state=42)
# Train logistic regression on reduced data
model = LogisticRegression()
model.fit(X_train, y_train)
# Predict and evaluate
y_pred = model.predict(X_test)
print("Accuracy with PCA-reduced data:", accuracy_score(y_test, y_pred))
Output: -
 
