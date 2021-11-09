function addTextMessage(color, message) {
    let msg = document.createElement('div');
    msg.innerHTML = '<div class="card full-border border-sirius-' + color + '-dark mb-4"><div class="card-body msgContent"></div></div>'
    msg.querySelector('.msgContent').textContent = message;
    document.querySelector('#message-box').appendChild(msg)
}

function addHtmlMessage(color, htmlMessage) {
    let msg = document.createElement('div');
    msg.innerHTML = '<div class="card full-border border-sirius-' + color + '-dark mb-4"><div class="card-body msgContent"></div></div>'
    msg.querySelector('.msgContent').innerHTML = htmlMessage;
    document.querySelector('#message-box').appendChild(msg)
}

function addErrorMessage(message) {
    addTextMessage('red', message);
}

function addSuccessMessage(message) {
    addTextMessage('green', message);
}

function addInfoMessage(message) {
    addTextMessage('blue', message);
}

function clearMessages() {
    document.querySelector('#message-box').innerHTML = '';
}
