var request = require('request');

request({
  url: 'http://localhost:8080/storeMessage',
  method: 'POST',
  json: {name: 'owen', message: 'balls!'}
}, function(error, response, body){
    if(error) {
      console.log(error);
    } else {
      console.log(response.statusCode, body);
    }
});

request({
  url: 'http://localhost:8080/searchMessage',
  method: 'GET',
  qs: {name: 'owen'},
}, function(error, response, body){
    if(error) {
      console.log(error);
    } else {
      console.log(response.statusCode, body);
    }
});
