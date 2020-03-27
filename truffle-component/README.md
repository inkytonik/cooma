# Cooma language component for the GraalVM

Truffle languages can be packaged as components which can be installed into
GraalVM using the [Graal
updater](http://www.graalvm.org/docs/reference-manual/graal-updater/). 

To generate the GraalVM Cooma component, run the ```buildComponent``` sbt task.

This gerates the graal compatible JAR component under path:

    cooma/truffle-component/cooma-component.jar
    
##Getting GraalVM Enterprise

Download GraalVM from Oracle OTP from https://www.oracle.com/downloads/graalvm-downloads.html
    
After downloading, add the ```bin``` folder to the PATH.

Running ```gu list``` should produce something similar to this:


    ComponentId              Version             Component name      Origin 
    --------------------------------------------------------------------------------
    graalvm                  20.0.0              GraalVM Core        
    native-image             20.0.0              Native Image   

The native-image component has to be installed separately. Explained further in the document.
   
   
Make sure ```JAVA_HOME``` is pointing to the GraalVM bin folder.

    .../graalvm-ee-java8-20.0.0/bin
   
## Install component

To install the component you can either install it manually using graalvm updater command ```gu```:
For both, you will need to add ```gu``` to the classpath.

### Manually
Run the following command on the ```cooma/truffle-component``` folder.

    gu install -L -r -f cooma-component.jar

To check that Cooma is now a component of GraalVM, run: ```gu list``` and should produce the following output:

    ComponentId              Version             Component name      Origin 
    --------------------------------------------------------------------------------
    org.bitbucket.inkytonik.c20.0.0              Cooma IR Language   
    graalvm                  20.0.0              GraalVM Core        
    native-image             20.0.0              Native Image    

### SBT task    

Run the sbt task ```installGraalVMComponent```.

### Component structure    

The cooma GraalVM component has the following structure.
 
    .
    ├── jre
    │   └── languages
    │       └── cooma
    │           ├── bin
    │           │   └── cooma           Main bash script that launches Cooma on the truffle frontend
    │           └── truffle_root.jar    Truffle implementation project fat jar
    └── META-INF
        ├── MANIFEST.MF
        ├── permissions                 Sets the permissions for the cooma executable file under Graal folder
        └── symlinks                    Sets the location of the cooma symlink


Installing Cooma component will create a symlink ```cooma``` on the ```bin``` folder of GraalVM.

To run Cooma on GraalVM, simple run the file under the bin folder, such as:

    .../graalvm-ee-java8-20.0.0/bin/cooma
    
## Install native-image component

1. Download ```native-image``` component from https://www.oracle.com/downloads/graalvm-downloads.html
2. Install the component using the following command:
    
    ```bash 
    ./gu install --file [path to native image component JAR file]
   ``` 
   The filename format is: ```native-image-installable-svm-svmee-java[Java version]-[OS]-amd64-[graalvm version].jar```
    