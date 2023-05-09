
// Swipe Up / Down / Left / Right
var initialX = null;
var initialY = null;

export const setup = (app) => {
    document.addEventListener("touchstart", startTouch, false);
    document.addEventListener("touchmove", moveTouch, false);

    function moveTouch(e) {
        if (initialX === null) {
            return;
        }

        if (initialY === null) {
            return;
        }

        var currentX = e.touches[0].clientX;
        var currentY = e.touches[0].clientY;

        var diffX = initialX - currentX;
        var diffY = initialY - currentY;

        if (Math.abs(diffX) > Math.abs(diffY)) {
            // sliding horizontally
            if (diffX > 0) {
                app.ports.swipe.send('left');
            } else {
                app.ports.swipe.send('right');
            }
        } else {
            // sliding vertically
            if (diffY > 0) {
                app.ports.swipe.send('up');
            } else {
                app.ports.swipe.send('down');
            }
        }

        initialX = null;
        initialY = null;

        e.preventDefault();
    };
}

function startTouch(e) {
    initialX = e.touches[0].clientX;
    initialY = e.touches[0].clientY;
    e.preventDefault();
};

