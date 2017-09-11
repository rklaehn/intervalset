#!/bin/bash
export CLASSPATH=''
export CLASSPATH=/Users/rklaehn/projects_git/intervalset/.jvm/target/scala-2.12/classes/:$CLASSPATH
export CLASSPATH=/Users/rklaehn/projects_git/intervalset/.jvm/target/scala-2.12/test-classes/:$CLASSPATH
export CLASSPATH=/Users/rklaehn/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.12.1.jar:$CLASSPATH
export CLASSPATH=/Users/rklaehn/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.1.jar:$CLASSPATH
echo $CLASSPATH
sbt intervalsetJVM/test:compile
java -XX:+UnlockDiagnosticVMOptions -XX:+PrintAssembly com.rklaehn.interval.DisassemblyTestApp
