BDA PRACTICAL NOTES (Consolidated)

------------------------------------------------------------
Practical 2: Implement Pig Program using PySpark
------------------------------------------------------------
from pyspark.sql import SparkSession
from pyspark.sql import Row

spark = SparkSession.builder.appName("Implement Pig using PySpark").getOrCreate()

data = [
    Row(name="Madhav", age=30),
    Row(name="Deepak", age=33),
    Row(name="Akshay", age=35)
]

df = spark.createDataFrame(data)
df.show()
df.filter(df.age > 30).show()
df.groupBy("age").count().show()

spark.stop()

Explanation:
Pig performs data manipulation like filtering, grouping, etc. In PySpark, similar operations are done using DataFrames and SQL functions.

------------------------------------------------------------
Practical 3: Implement Word Count Frequency Program using MapReduce
------------------------------------------------------------
from pyspark import SparkContext

sc = SparkContext("local", "WordCountExample")
data = sc.textFile("data.txt")

wordCounts = data.flatMap(lambda line: line.split(" ")).map(lambda word: (word, 1)).reduceByKey(lambda a, b: a + b)
for word, count in wordCounts.collect():
    print(f"{word}: {count}")

sc.stop()

Explanation:
Implements MapReduce in PySpark using flatMap, map, and reduceByKey transformations.

------------------------------------------------------------
Practical 4: Configure Hive and Implement Application in Hive
------------------------------------------------------------
from pyspark.sql import SparkSession
from pyspark.sql import Row

spark = SparkSession.builder.appName("Hive with PySpark Example").enableHiveSupport().getOrCreate()

data = [
    Row(name="Madhav", age=30),
    Row(name="Deepak", age=33),
    Row(name="Akshay", age=35)
]

df = spark.createDataFrame(data)

spark.sql("CREATE DATABASE IF NOT EXISTS sample_db")
spark.sql("USE sample_db")

df.write.mode("overwrite").saveAsTable("people")

result = spark.sql("SELECT * FROM people")
result.show()

spark.stop()

Explanation:
Demonstrates Hive integration with Spark for creating databases, storing DataFrames, and querying tables using SQL.

------------------------------------------------------------
Practical 5: Implement Spark SQL
------------------------------------------------------------
from pyspark.sql import SparkSession

spark = SparkSession.builder.appName("SparkSQLExample").getOrCreate()
df = spark.read.csv("housing.csv", header=True, inferSchema=True)
df.createOrReplaceTempView("housing")

spark.sql("SELECT ocean_proximity, AVG(median_income) FROM housing GROUP BY ocean_proximity").show()
spark.sql("SELECT * FROM housing WHERE median_house_value > 300000").show()

spark.stop()

Explanation:
Demonstrates Spark SQL to analyze structured datasets using SQL queries.

------------------------------------------------------------
Practical 6: Implement Machine Learning in Spark
------------------------------------------------------------
a) Linear Regression
from pyspark.ml.regression import LinearRegression
from pyspark.sql import SparkSession
from pyspark.ml.feature import VectorAssembler

spark = SparkSession.builder.appName("LinearRegressionExample").getOrCreate()
data = spark.read.csv("linear_regression_data.csv", header=True, inferSchema=True)

assembler = VectorAssembler(inputCols=["feature"], outputCol="features")
data = assembler.transform(data)

lr = LinearRegression(featuresCol="features", labelCol="label")
model = lr.fit(data)

print("Coefficients:", model.coefficients)
print("Intercept:", model.intercept)

spark.stop()

b) Logistic Regression
from pyspark.ml.classification import LogisticRegression
from pyspark.sql import SparkSession
from pyspark.ml.feature import VectorAssembler

spark = SparkSession.builder.appName("LogisticRegressionExample").getOrCreate()
data = spark.read.csv("logistic_regression_data.csv", header=True, inferSchema=True)

assembler = VectorAssembler(inputCols=["feature1", "feature2"], outputCol="features")
data = assembler.transform(data)

lr = LogisticRegression(featuresCol="features", labelCol="label")
model = lr.fit(data)

print("Coefficients:", model.coefficients)
print("Intercept:", model.intercept)

spark.stop()

Explanation:
Linear Regression predicts continuous values, Logistic Regression predicts binary outcomes.

------------------------------------------------------------
Practical 7: Implement Spark Streaming
------------------------------------------------------------
from pyspark.streaming import StreamingContext
from pyspark import SparkContext

sc = SparkContext("local[2]", "NetworkWordCount")
ssc = StreamingContext(sc, 5)

lines = ssc.socketTextStream("localhost", 9999)
words = lines.flatMap(lambda line: line.split(" "))
pairs = words.map(lambda word: (word, 1))
wordCounts = pairs.reduceByKey(lambda a, b: a + b)
wordCounts.pprint()

ssc.start()
ssc.awaitTermination()

Explanation:
Processes real-time streaming data using PySpark Streaming API.

------------------------------------------------------------
Practical 8: Demonstrate Spark Shell Commands
------------------------------------------------------------
!pip install pyspark

from pyspark.sql import SparkSession
spark = SparkSession.builder.appName("SparkShellDemo").getOrCreate()

print("Spark version:", spark.version)
print("App Name:", spark.sparkContext.appName)
print("Master:", spark.sparkContext.master)

data = spark.sparkContext.textFile("data.txt")
print("Total lines:", data.count())

words = data.flatMap(lambda line: line.split(" "))
wordCounts = words.map(lambda word: (word, 1)).reduceByKey(lambda a, b: a + b)
for w in wordCounts.collect():
    print(w)

from pyspark.sql import Row
df = spark.createDataFrame([
    Row(id=1, name="Alice", age=23),
    Row(id=2, name="Bob", age=25),
    Row(id=3, name="Cathy", age=29)
])
df.show()

df.createOrReplaceTempView("people")
spark.sql("SELECT name FROM people WHERE age > 24").show()

spark.stop()

Explanation:
Demonstrates Spark shell-like operations using RDDs, DataFrames, and SQL in PySpark.

------------------------------------------------------------
Practical 9: Implement Decision Tree
------------------------------------------------------------
from pyspark.ml.classification import DecisionTreeClassifier
from pyspark.ml.feature import VectorAssembler
from pyspark.sql import SparkSession

spark = SparkSession.builder.appName("DecisionTreeExample").getOrCreate()
data = spark.read.csv("decision_tree_data.csv", header=True, inferSchema=True)

assembler = VectorAssembler(inputCols=["feature1", "feature2"], outputCol="features")
data = assembler.transform(data)

dt = DecisionTreeClassifier(labelCol="label", featuresCol="features")
model = dt.fit(data)

predictions = model.transform(data)
predictions.select("features", "label", "prediction").show()

spark.stop()

Explanation:
Decision Tree algorithm classifies data based on features, used for predictive analytics.
