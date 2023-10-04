// Enable buttons after half a second
setTimeout(function() {
    document.querySelectorAll('button').forEach(function(button) {
        button.disabled = false;
    });
}, 500);
