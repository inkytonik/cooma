  $ java -jar "$TESTDIR/../../../target/scala-2.13/cooma.jar" -r "$TESTDIR/http-server.cooma" 8080 < /dev/null > /dev/null 2>&1 &
  $ sleep 1
  $ COOMA_PID="$!"

Lorem:

  $ curl -s http://localhost:8080/lorem && echo
  Lorem ipsum dolor sit amet

Echo:

  $ curl -s http://localhost:8080/echo -d "Echo!" && echo
  Echo!

Hello:

  $ curl -s http://localhost:8080/hello -d "Tony" && echo
  Hello Tony

Not found:

  $ curl -LI http://localhost:8080/qwerty -o /dev/null -w '%{http_code}\n' -s
  404

  $ kill "$COOMA_PID"
