name := "DataGeneration"
version := "0.1"

scalaVersion := "2.12.15"

projectDependencies ++= {
  Seq(
    "org.apache.spark" %% "spark-sql" % "3.3.2",
    "org.apache.spark" %% "spark-core" % "3.3.2"
  )
}

