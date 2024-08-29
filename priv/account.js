// Function to show the loading spinner
function showLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'block';
}

// Function to hide the loading spinner
function hideLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'none';
}

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
    const responseDiv = document.getElementById('response-div');
    if (jsonData.type == 'error') {
	const errorHTML = '<p>'+jsonData.message+'</p>'
	console.log(errorHTML);
	responseDiv.insertAdjacentHTML('beforeend', errorHTML);
	hideLoadingSpinner();
    }else if (jsonData.type == 'verify'){
	const newHTML = "<div class='input-group'><label for='verification-code'>Verification code</label><input type='number' id='verification-code' name='verification-code' required/></div>";
	responseDiv.innerHTML = newHTML;
	hideLoadingSpinner();
    }else {
	localStorage.setItem('ninxai-cookie', jsonData.authToken);
	const container = document.querySelector('.container');
	container.style.display = 'none';
	socket.close();
	// redirect to paypal
	var choice = window.confirm("You will now be redirected to Paypal to deposit a trial balance. Payment takes upto five minutes to reflect on your balance.");
	choice ? window.location.href = 'https://www.paypal.com/ncp/payment/M49NXZ7P8PJGL' : false;
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
    showLoadingSpinner();
	  console.log('sending form');
    const email = document.getElementById('email');
    const password = document.getElementById('password');
    const code = document.getElementById('verification-code');
    if (!code){
	socket.send(JSON.stringify({type: 'login/signup', email: email.value, password: password.value}));
    }else{
	socket.send (JSON.stringify({type: 'verify-code', email: email.value, password: password.value, code: code.value}))
    }
});
