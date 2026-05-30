(function() {
  function updateTrainSlotColors() {
    var controls = document.querySelectorAll('.train-qty-control');
    if (!controls.length) return;
    var total = 0;
    controls.forEach(function(control) {
      var input = control.querySelector('input[id^="train_"]');
      if (!input) return;
      var value = parseFloat(input.value);
      if (!isNaN(value)) total += value;
    });
    var state = 'empty';
    if (total > 6) state = 'over';
    else if (total === 6) state = 'ready';
    else if (total > 0) state = 'under';
    controls.forEach(function(control) {
      control.classList.remove('train-slot-empty', 'train-slot-under', 'train-slot-ready', 'train-slot-over');
      control.classList.add('train-slot-' + state);
    });
  }
  document.addEventListener('input', function(event) {
    if (event.target && event.target.closest('.train-qty-control')) updateTrainSlotColors();
  });
  document.addEventListener('change', function(event) {
    if (event.target && event.target.closest('.train-qty-control')) updateTrainSlotColors();
  });
  document.addEventListener('DOMContentLoaded', updateTrainSlotColors);
  document.addEventListener('shiny:bound', updateTrainSlotColors);
  document.addEventListener('shiny:value', function() {
    window.setTimeout(updateTrainSlotColors, 0);
  });
  document.addEventListener('shiny:connected', function() {
    if (window.Shiny) {
      Shiny.addCustomMessageHandler('updateTrainSlotColors', function(message) {
        [0, 50, 150, 300].forEach(function(delay) {
          window.setTimeout(updateTrainSlotColors, delay);
        });
      });
    }
  });
})();
