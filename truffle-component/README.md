# Cooma language component for the GraalVM

Truffle languages can be packaged as components which can be installed into
GraalVM using the [Graal
updater](http://www.graalvm.org/docs/reference-manual/graal-updater/). 
Running `mvn package` in the simplelanguage folder also builds a
`cooma-component.jar`. 
This file is the simple languages component for GraalVM and can be installed by
running

```
/path/to/graalvm/bin/gu install /path/to/cooma-component.jar
```

