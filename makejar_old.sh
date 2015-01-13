echo "This creates a .jar runnable by java -jar on systems without scala."

export CLASSPATH="$CLASSPATH:`pwd`:`pwd`/src/main/scala"
export CLASSPATH="$CLASSPATH:`pwd`/components/commons-collections-3.2.1.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/commons-configuration-1.6.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/commons-lang-2.5.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/commons-logging-1.1.1.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/HCMInterface.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.core.contenttype_3.4.1.R35x_v20090826-0451.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.core.jobs_3.4.100.v20090429-1800.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.core.resources_3.5.2.R35x_v20091203-1235.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.core.runtime_3.5.0.v20090525.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.equinox.common_3.5.1.R35x_v20090807-1100.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.equinox.preferences_3.2.301.R35x_v20091117.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.jdt.core_3.5.2.v_981_R35x.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/org.eclipse.osgi_3.5.2.R35x_v20100126.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/sbt-launch.jar"
export CLASSPATH="$CLASSPATH:`pwd`/components/scalatest_2.11-2.2.1.jar"
export JAVA_OPTS="-Xmx4G"

# compile the main tree
echo "Compiling src/main/scala"
(
    cd src/main/scala
    scalac -verbose `find -name "*.scala"`
)