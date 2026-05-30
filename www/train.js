(function () {

    /**
     * Helper function updates the colors of select inputs
     * for each train car item slot tile in the grid
     * */ 
    function updateTrainSlotColors() {

        // Get all train slot controls to sum slots >0
        var controls = document.querySelectorAll('.train-qty-control');
        if (!controls.length) return;
        var total = 0;

        // Sum total # items selected
        controls.forEach(function (control) {
            var input = control.querySelector('input[id^="train_"]');
            if (!input) return;
            var value = parseFloat(input.value);
            if (!isNaN(value)) total += value;
        });

        // Set state based on # items selected across all slots
        var state = 'empty';
        if (total > 6) state = 'over';
        else if (total === 6) state = 'ready';
        else if (total > 0) state = 'under';

        // Clear existing state classes and apply new state class to all controls
        controls.forEach(function (control) {
            control.classList.remove(
                'train-slot-empty',
                'train-slot-under',
                'train-slot-ready',
                'train-slot-over',
            );
            control.classList.add('train-slot-' + state);
        });
    }

    // Attach event listeners to update slot colors on input changes by user
    document.addEventListener('input', function (event) {
        if (event.target && event.target.closest('.train-qty-control')) updateTrainSlotColors();
    });
    document.addEventListener('change', function (event) {
        if (event.target && event.target.closest('.train-qty-control')) updateTrainSlotColors();
    });

    //Initial call to update colors on page load
    document.addEventListener('DOMContentLoaded', updateTrainSlotColors);

    //Listener for Shinyapps events to update colors when train data changes
    document.addEventListener('shiny:bound', updateTrainSlotColors);
    document.addEventListener('shiny:value', function () {
        window.setTimeout(updateTrainSlotColors, 0);
    });
    document.addEventListener('shiny:connected', function () {
        if (window.Shiny) {
            Shiny.addCustomMessageHandler('updateTrainSlotColors', function (message) {
                [0, 50, 150, 300].forEach(function (delay) {
                    window.setTimeout(updateTrainSlotColors, delay);
                });
            });
        }
    });
})();
