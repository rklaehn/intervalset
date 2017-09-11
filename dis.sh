#!/bin/bash
export PWD=`pwd`
export CLASSPATH=''
export CLASSPATH=$PWD/.jvm/target/scala-2.12/classes/:$CLASSPATH
export CLASSPATH=$PWD/.jvm/target/scala-2.12/test-classes/:$CLASSPATH
export CLASSPATH=$HOME/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.12.1.jar:$CLASSPATH
export CLASSPATH=$HOME/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.12.1.jar:$CLASSPATH
sbt intervalsetJVM/test:compile
java -XX:+UnlockDiagnosticVMOptions -XX:CompileCommand=print,*Methods$.* com.rklaehn.interval.DisassemblyTestApp
