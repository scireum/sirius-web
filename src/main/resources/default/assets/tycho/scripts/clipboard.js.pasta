function copyToClipboard(value) {
    if (!navigator.clipboard) {
        fallbackCopyToClipboard(value);
    } else {
        navigator.clipboard.writeText(value)
    }
}

function fallbackCopyToClipboard(value) {
    const fakeElem = document.createElement('textarea');
    fakeElem.value = value;
    document.body.appendChild(fakeElem);
    fakeElem.select();
    document.execCommand('copy');
    document.body.removeChild(fakeElem);
}
