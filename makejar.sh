echo "This creates a .jar runnable by java -jar on systems without scala."

set -e

# so what to target?
select main in $(grep -l "main" `find src/main/scala/nl -name "*.scala"` | sed 's:/:.:g;s:^.*[.]nl:nl:;s:[.]scala$::'); do
	break
done


export CLASSPATH="$CLASSPATH:`pwd`:`pwd`/src/main/scala:`pwd`/src/test/scala:`pwd`/components/interface/HCMInterface.jar"

echo "Compiling HCM Interface"
(
	./compile_hcm_interface.sh
)

# compile the java components
echo "Compiling components/"
(
    cd components/javacomponents
    javac -classpath ../../components/interface/HCMInterface.jar `find -name "*.java" | grep -v SBT`
)

# compile the main tree
echo "Compiling src/main/scala"
(
    cd src/main/scala    
    javac `find -name "*.java" | grep -v SBT`
    scalac `find -name "*.scala" | grep -v SBT`
)

# try to locate the scala-library
echo "Locating scala-library..."
LIB=~/scala/lib/scala-library.jar
LIB2=~/scala/lib/scala-compiler.jar
LIB3=~/scala/lib/scala-reflect.jar
LIB4=~/shared/eca/components/interface/HCMInterface.jar
#test -f $LIB || LIB=`which scala | grep -o '/.*scala[^\/]*/bin'`/../lib/scala-library.jar

if [ ! -f "$LIB" ]; then
    echo "Error: scala-library.jar not found!"
    exit 2
fi

if [ ! -f "$LIB2" ]; then
    echo "Error: scala-compiler.jar not found!"
    exit 2
fi

if [ ! -f "$LIB3" ]; then
    echo "Error: scala-reflect.jar not found!"
    exit 2
fi

if [ ! -f "$LIB4" ]; then
    echo "Error: HCMInterface.jar not found!"
    exit 2
fi

# extract the entire scala runtime
(
    cd /tmp
    jar xf "$LIB" 
    jar xf "$LIB2"
    jar xf "$LIB3"
)

# extract the HCMInterface stuff
(
    mkdir hcmtmp
    cd hcmtmp
    jar xf "$LIB4"
)

# create a jar archive of our project + the scala library
jar cef "$main" ecalogic.jar -C src/main/scala nl -C /tmp scala -C hcmtmp hcminterface
echo "Jar created!"

# clean up
rm -rf /tmp/scala
rm -rf hcmtmp
