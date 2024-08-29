// if the user is loggedin, redirect them to the chat page
localStorage.getItem('ninxai-cookie') ? window.location.href = "/chat" : false;

// toggle Password visiblity
function togglePasswordVisibility() {
    var passwordField = document.getElementById("password");
    var toggleCheckbox = document.getElementById("toggle-password");
    if (toggleCheckbox.checked) {
	passwordField.type = "text";
    } else {
	passwordField.type = "password";
    }
}


// HANDLING OF WEBSOKCET CONNECTION FOR CHATS.
// Create a new WebSocket connection to the server
const socket = new WebSocket('wss://localhost:8443/ws');

const debug = true;

// Connection opened
socket.addEventListener('open', (event) => {
    debug ? console.log('Connected to WebSocket server') : false;
    // send the authentification token to the server
    //socket.send('Hello Server!');
});

// handle messages
socket.addEventListener('message', (event) => {
    debug ? console.log('Message from server ', event.data) : false;
    const jsonData = JSON.parse(event.data);
    console.log(jsonData.type);
    const responseDiv = document.getElementById('response-div');
    if (jsonData.type == 'code-sent'){
	const newHTML = "<div class='input-group'><label for='verification-code'>Verification code</label><input type='number' id='verification-code' name='verification-code' required/></div>";
	responseDiv.innerHTML = '';
	      responseDiv.insertAdjacentHTML('beforebegin', newHTML);
    }else  if (jsonData.type == 'error') {
	const errorHTML = '<p>'+jsonData.message+'</p>'
	console.log(errorHTML);
	responseDiv.innerHTML = errorHTML;
    }else if (jsonData.type == 'reset-password'){
	const newHTML = "<div class='input-group'><label for='password'>Password</label><input type='password' id='password' name='password' required/></div>";
	responseDiv.innerHTML = '';
	      responseDiv.insertAdjacentHTML('beforebegin', newHTML);
    }else {
	socket.close();
	window.location.href = '/';
    }
});

// Handle errors
socket.addEventListener('error', (event) => {
    debug ? console.error('WebSocket error: ', event) : false;
});

// Connection closed
socket.addEventListener('close', (event) => {
    debug ? console.log('WebSocket connection closed') : false;
});

document.getElementById('account-form').addEventListener('submit', (event) =>{
    event.preventDefault();
    console.log('sending form');
    const email = document.getElementById('email');
    const password = document.getElementById('password');
    const code = document.getElementById('verification-code');
    if (email && !password && !code){
	socket.send(JSON.stringify({type: 'reset-password/verify-email', email: email.value}));
    } else if (email && code && !password){
	socket.send(JSON.stringify({type: 'reset-password/verify-code', email: email.value, code: code.value}));
    }else{
	socket.send(JSON.stringify({type: 'reset-password/reset', email: email.value, password: password.value, code: code.value}))
    }
});
