$(function () {
  var currSlide = 0;
  var allSlides = $('.slide');

  // Initialize: Hide all

  $('.slide')
    .removeClass('active')
    .addClass('inactive');

  $(allSlides[currSlide])
    .removeClass('inactive')
    .addClass('active');

  // Update

  $(document).on('keydown', function (event) {
    var nextSlide = currSlide;

    // https://css-tricks.com/snippets/javascript/javascript-keycodes/
    
    // page up
    if (event.keyCode === 33) { 
      console.log('previous slide');
      nextSlide = currSlide ? currSlide - 1 : currSlide;
    // page down
    } else if (event.keyCode === 34) { 
      console.log('next slide');
      nextSlide = currSlide < allSlides.length - 1 ? currSlide + 1 : currSlide;
    }

    if (nextSlide !== currSlide) {

      $(allSlides[currSlide])
        .removeClass('active')
        .addClass('inactive');
      $(allSlides[nextSlide])
        .removeClass('inactive')
        .addClass('active');

      currSlide = nextSlide;
    }
    event.preventDefault();
  });
});
