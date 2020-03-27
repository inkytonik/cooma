# Native image generation for Cooma.


While the truffle-component is the polyglot implementation of Cooma, using the Truffle framework and it is a GraalVM compatible component, the native image is completely different.

GraalVM, with native-image, allows the AOT compilation of programs, reducing the startup and execution times of programs running on JVM. 
This has some limitations, though, such as reflection usage.

As Cooma uses both Scala and Java with the help of libraries, we need to first analyze the reflection usage.


## Generating the necessary files

### Pre-requisites

1. Download GraalVM
2. Install native-image component
3. Add Graalvm bin folder to path
3. Set JAVA_HOME to Graalvm's bin path

For these steps, please refer to [truffle component documentation](../truffle-component/README.md) .

### Generating Cooma fat jar

Run the sbt task ```assembly```

### Run java agent

While on the cooma root folder, run the following command, making sure the scala version is the correct one:

    "$JAVA_HOME"/bin/java -agentlib:native-image-agent=config-output-dir=truffle-native/src/main/resources/config -jar target/scala-2.13/cooma.jar

This will generate the files in the ```truffle-native``` project:

    truffle-native
    ├── README.md
    ├── src
    │   └── main
    │       └── resources
    │           └── config
    │               ├── jni-config.json
    │               ├── proxy-config.json
    │               ├── reflect-config.json
    │               └── resource-config.json


##Generating the native image.

To generate the native image, execute the following command on the cooma root folder:

    $JAVA_HOME"/bin/native-image  --allow-incomplete-classpath --report-unsupported-elements-at-runtime --no-fallback --initialize-at-build-time -H:+ReportExceptionStackTraces -H:ReflectionConfigurationFiles="truffle-native/src/main/resources/config/reflect-config.json" -cp target/scala-2.13/cooma.jar org.bitbucket.inkytonik.cooma.Main coomanative   

Should produce an output like this:

    Build on Server(pid: 1781, port: 39445)
    [coomanative:1781]    classlist:   5,693.91 ms,  2.77 GB
    [coomanative:1781]        (cap):     718.28 ms,  2.77 GB
    [coomanative:1781]        setup:   1,346.27 ms,  2.77 GB
    [coomanative:1781]   (typeflow):  24,706.50 ms,  2.87 GB
    [coomanative:1781]    (objects):  21,279.39 ms,  2.87 GB
    [coomanative:1781]   (features):     784.39 ms,  2.87 GB
    [coomanative:1781]     analysis:  48,594.89 ms,  2.87 GB
    [coomanative:1781]     (clinit):   1,305.07 ms,  2.87 GB
    [coomanative:1781]     universe:   3,292.23 ms,  2.87 GB
    [coomanative:1781]      (parse):   4,836.16 ms,  2.87 GB
    [coomanative:1781]     (inline):  14,561.22 ms,  3.78 GB
    [coomanative:1781]    (compile):  77,666.86 ms,  4.16 GB
    [coomanative:1781]      compile:  99,539.92 ms,  4.16 GB
    [coomanative:1781]        image:   6,191.32 ms,  4.18 GB
    [coomanative:1781]        write:  15,862.69 ms,  4.18 GB
    [coomanative:1781]      [total]: 180,781.90 ms,  4.18 GB

Running the native file:

    diego@ET2321I:~/dev/cooma$ ./coomanative

For the moment produces the following output:

    Exception in thread "main" com.oracle.svm.core.jdk.UnsupportedFeatureError: Invoke with MethodHandle argument could not be reduced to at most a single call or single field access. The method handle must be a compile time constant, e.g., be loaded from a `static final` field. Method that contains the method handle invocation: java.lang.invoke.LambdaForm$MH/137312629.invoke_MT(Object, Object)
        at com.oracle.svm.core.util.VMError.unsupportedFeature(VMError.java:101)
        at scala.runtime.Statics.releaseFence(Statics.java:148)
        at scala.reflect.api.Universe.<init>(Universe.scala:73)
        at scala.reflect.macros.Universe.<init>(Universe.scala:33)
        at scala.reflect.internal.SymbolTable.<init>(SymbolTable.scala:28)
        at scala.reflect.runtime.JavaUniverse.<init>(JavaUniverse.scala:30)
        at scala.reflect.runtime.package$.universe$lzycompute(package.scala:29)
        at scala.reflect.runtime.package$.universe(package.scala:29)
        at org.bitbucket.inkytonik.kiama.util.Config$$anon$1.<init>(Config.scala:52)
        at org.bitbucket.inkytonik.kiama.util.Config.<init>(Config.scala:36)
        at org.bitbucket.inkytonik.kiama.util.REPLConfig.<init>(Config.scala:187)
        at org.bitbucket.inkytonik.cooma.Config.<init>(Config.scala:15)
        at org.bitbucket.inkytonik.cooma.Main$.main(Main.scala:19)
        at org.bitbucket.inkytonik.cooma.Main.main(Main.scala)

For the moment, it seems that there is a scala feature that is not supported by the native image compiler.
We expect the next versions of Scala fix this.