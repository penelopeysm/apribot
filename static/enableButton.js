// Enable buttons after 2 seconds
setTimeout(function() {
    document.querySelectorAll('button').forEach(function(button) {
        button.disabled = false;
    });
}, 2000);
