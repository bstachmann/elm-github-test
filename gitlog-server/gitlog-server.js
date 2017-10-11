var http = require("http");
var sys = require('util');
var child_process = require('child_process');
const { URL } = require('url');

http.createServer(function (request, response) {

   // Send the HTTP header
   // HTTP Status: 200 : OK
   // Content Type: text/plain
   response.writeHead(200, {'Content-Type': 'text/plain'});

   var output = "<no response>";

   output = child_process.execSync("git log --oneline", {});

   response.write('hullatrulla \n');
   response.write('url '+ request.url +'\n');
   var sp = (new URL(request.url, 'https://example.org/')).searchParams
    response.write('parans' + sp.toString() + '\n');
   response.write('K  '+sp.get( 'kaese') + '\n');
   response.write('headers '+request.headers+' \n');
   response.write('hullatrulla \n');
   response.end('Hello World. Output is ' + output+ '\n');

}).listen(8081);

// Console will print the message
console.log('Server running at http://127.0.0.1:8081/');
