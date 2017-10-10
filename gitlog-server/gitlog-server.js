var http = require("http");
var sys = require('util')
var child_process = require('child_process');

http.createServer(function (request, response) {

   // Send the HTTP header
   // HTTP Status: 200 : OK
   // Content Type: text/plain
   response.writeHead(200, {'Content-Type': 'text/plain'});

   var output = "<no response>";

  //  var child = exec("pwd", function (error, stdout, stderr) {
   //
  //    console.log('stdout: ' + stdout);
  //    console.log('stderr: ' + stderr);
   //
  //    if (error !== null) {
  //      console.log('exec error: ' + error);
  //    } else {
  //      output = stdout;
  //    }
   //
  //  });

   output = child_process.execSync("git log --oneline", {});

   response.write('hullatrulla \n');
   response.write('url '+ request.url +'\n');
   response.write('hullatrulla \n');
   response.write('headers '+request.headers+' \n');
   response.write('hullatrulla \n');
   response.end('Hello World. Output is ' + output+ '\n');

}).listen(8081);

// Console will print the message
console.log('Server running at http://127.0.0.1:8081/');
