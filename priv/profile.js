const debug = true;

// Function to show the loading spinner
function showLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'block';
}

// Function to hide the loading spinner
function hideLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'none';
}

// HANDLING OF WEBSOKCET CONNECTION FOR CHATS.
// Create a new WebSocket connection to the server
const socket = new WebSocket('wss://localhost:8443/ws');

// Connection opened
socket.addEventListener('open', (event) => {
    showLoadingSpinner();
    debug ? console.log('Connected to WebSocket server') : false;
    // the chat is not registered when it is created because there is no easy of using localStorage then, but if the value is set, then upon openning
    // a new socket, that chatId on the chat form should be sent to the server for registering.
    const cookie = localStorage.getItem('ninxai-cookie');
    socket.send(JSON.stringify({type: 'profile', cookie: cookie}));
});

// Listen for messages from the server
socket.addEventListener('message', (event) => {
    debug ? console.log('Message from server ', event.data) : false;
    jsonData = JSON.parse(event.data);
    if (jsonData.type == "details"){
	const email = jsonData.email;
	const password = jsonData.password;
	const tokenCounts = jsonData['token-counts'];
	const emailInput = document.getElementById('email');
	const passwordInput = document.getElementById('password');
	emailInput.value = email;
	passwordInput.value = password;
	const tcb = document.getElementById('tct-body');
	var newHTML = '';
	Object.keys(tokenCounts).forEach((key) => newHTML = newHTML+'<tr><td>'+key+'</td><td>'+tokenCounts[key][0]+'</td><td>'+tokenCounts[key][1]+'</td></tr>');
	tcb.insertAdjacentHTML('beforeend', newHTML);
	const totalTokens = jsonData['total-cost'];
	document.getElementById('total-cost').innerHTML = totalTokens;
	hideLoadingSpinner();
    }else if(jsonData.type = "chat-zip"){
	const zip = jsonData.zip;
	const filename = jsonData.filename;
	const button = document.querySelector('.download-chats');
	const newHTML = "<a download="+filename+" style='text-decoration:none; color:white' href='data:application/zip;base64,"+zip+"'>Save Now</a>";
	button.innerHTML = newHTML;
	hideLoadingSpinner();
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


// handle logout Button
document.querySelector('.logout-btn').addEventListener('click', (event) =>{
    event.preventDefault();
    const ninxaiCookie = localStorage.getItem('ninxai-cookie');
    localStorage.removeItem('ninxai-cookie');
    socket.send(JSON.stringify({type: 'logout', 'ninxai-cookie': ninxaiCookie}));
    socket.close();
    window.location.href = '/';
});

// handle download chats button.
document.querySelector('.download-chats').addEventListener('click', (event) => {
    showLoadingSpinner();
    const ninxaiCookie = localStorage.getItem('ninxai-cookie');
    socket.send(JSON.stringify({type: 'download-chats', cookie: ninxaiCookie}));
});

// reset password Link
document.querySelector('.reset-password').addEventListener('click', (event) =>{
    event.preventDefault();
    const ninxaiCookie = localStorage.getItem('ninxai-cookie');
    localStorage.removeItem('ninxai-cookie');
    socket.send(JSON.stringify({type: 'logout', 'ninxai-cookie': ninxaiCookie}));
    socket.close();
    window.location.href = '/reset-password';
});
