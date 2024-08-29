// Auto-grow the textarea as the user types
const textarea = document.querySelector('.chat-input textarea');
const debug = true;

textarea.addEventListener('input', function() {
    this.style.height = 'auto'; // Reset height to auto
    this.style.height = (this.scrollHeight) + 'px'; // Set new height
});

// control+enter for new line
textarea.addEventListener('keydown', function(event) {
    if (event.ctrlKey && event.key === 'Enter') {
	event.preventDefault();
	const textarea = this;
	const start = textarea.selectionStart;
	const end = textarea.selectionEnd;
	textarea.value = textarea.value.substring(0, start) + '\n' + textarea.value.substring(end);
	textarea.selectionStart = textarea.selectionEnd = start + 1;
	this.style.height = 'auto'; // Reset height to auto
	this.style.height = (this.scrollHeight) + 'px'; // Set new height
    }
});

function isTextareaNotEmpty() {
    const text = textarea.value.trim();
    return text.length > 0;
}

// submit form via websocket on hitting enter and textarea is not empty
textarea.addEventListener('keydown', function(event) {
    if (event.key === 'Enter' && isTextareaNotEmpty()) {
	event.preventDefault();
	const form = document.querySelector ('.chat-input');
	const message = document.querySelector('.chat-input textarea').value;
	const messagesDiv = document.querySelector('.chat-messages');
	const newDiv = document.createElement('div');
	newDiv.className = "message sent";
	newDiv.innerHTML = message;
	messagesDiv.append(newDiv);
	messagesDiv.scrollTop = messagesDiv.scrollHeight;
	const chatId = document.querySelector('.chat-input input').value;
	showLoadingSpinner(); // this will be hidden in message handler
	socket.send(JSON.stringify({message: message, chatId: chatId, type: 'chat'}));
	form.reset();
    }
});


// Function to show the loading spinner
function showLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'block';
}

// Function to hide the loading spinner
function hideLoadingSpinner() {
    document.getElementById('loading-spinner').style.display = 'none';
}

// typewriter effect
function typeWriterEffect(text, element, delay = 100) {
    let index = 0;
    const typing = setInterval(function() {
        if (index < text.length) {
            element.innerHTML += text.charAt(index);
            index++;
        } else {
            clearInterval(typing);
        }
    }, delay);
}

// HANDLING OF WEBSOKCET CONNECTION FOR CHATS.
// Create a new WebSocket connection to the server
const socket = new WebSocket('wss://localhost:8443/ws');

// Connection opened
socket.addEventListener('open', (event) => {
    debug ? console.log('Connected to WebSocket server') : false;
    // the chat is not registered when it is created because there is no easy of using localStorage then, but if the value is set, then upon openning
    // a new socket, that chatId on the chat form should be sent to the server for registering.
    showLoadingSpinner();
    const chatId = document.querySelector('.chat-input input').value;
    const cookie = localStorage.getItem('ninxai-cookie');
    socket.send(JSON.stringify({chatId: chatId, type: 'register chat', cookie: cookie}));
    hideLoadingSpinner();
});

// Listen for messages from the server
socket.addEventListener('message', (event) => {
    debug ? console.log('Message from server ', event.data) : false;
    jsonData = JSON.parse(event.data);
    if (jsonData.type == "reload"){
	location.reload(); // this happens when a new chat is registered at the server.
    }else if (jsonData.type == "title"){
	const title = document.getElementById('chat-title');
	title.innerHTML = jsonData.messages;
    }else if (jsonData.type == "no trial subscription") { // no trial subcription yet, send to trial link
	var choice = window.confirm("You have not added a trial balance to your account. Payment takes upto five minutes to reflect on your balance. Proceed?");
	choice ? window.location.href = 'https://www.paypal.com/ncp/payment/M49NXZ7P8PJGL' : false;
    }else if (jsonData.type == "no balance") { // no balance, send to balance link
	var choice = window.confirm("You don't have enough balance to continue in conversation! Payment takes upto five minutes to reflect on your balance. Do you want to proceed to Paypal to add some more?");
	choice ? window.location.href = 'https://www.paypal.com/ncp/payment/KTV8DNR28ZP9N' : false;
    }else{
	// Display the message on the webpage
	const messagesDiv = document.getElementById('chat-messages');
	const newChild = document.createElement('div');
	messagesDiv.insertAdjacentHTML('beforeend', jsonData.messages);
	messagesDiv.scrollTop = messagesDiv.scrollHeight;
	hideLoadingSpinner(); // this was shown by form submit event
    }
});

// Handle errors
socket.addEventListener('error', (event) => {
    debug ? console.error('WebSocket error: ', event) : false;
});

// Connection closed
socket.addEventListener('close', (event) => {
    debug ? console.log('WebSocket connection closed') : false;
    var choice = window.confirm('The connection to the server has closed unexpectedly. Do you want to reconnect?');
    choice ? window.location.reload() : false;
});

document.querySelector('.chat-input').addEventListener('submit', (event) => {
    event.preventDefault();
    const form = document.querySelector ('.chat-input');
    const message = document.querySelector('.chat-input textarea').value;
    const messagesDiv = document.querySelector('.chat-messages');
    const newDiv = document.createElement('div');
    newDiv.className = "message sent";
    newDiv.innerHTML = message;
    messagesDiv.append(newDiv);
    messagesDiv.scrollTop = messagesDiv.scrollHeight;
    const chatId = document.querySelector('.chat-input input').value;
    showLoadingSpinner(); // this will be hidden in message handler
    socket.send(JSON.stringify({message: message, chatId: chatId, type: 'chat'}));
    form.reset();
});

// this will scroll to the bottom of the chat-messages when the document is loaded.
const messagesDiv = document.querySelector('.chat-messages');
messagesDiv.scrollTop = messagesDiv.scrollHeight;

// handle logout Button
document.querySelector('.logout-btn').addEventListener('click', (event) =>{
    event.preventDefault();
    const ninxaiCookie = localStorage.getItem('ninxai-cookie');
    localStorage.removeItem('ninxai-cookie');
    socket.send(JSON.stringify({type: 'logout', 'ninxai-cookie': ninxaiCookie}));
    socket.close();
    window.location.href = '/';
});
