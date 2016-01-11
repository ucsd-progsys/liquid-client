// Super stripped down slideshow.

$(function () {
  var currSlide = 0;
  var allSlides = $('slidesection');

  // Initialize: Hide all

  $('slidesection')
    .removeClass('active')
    .addClass('inactive');

  $(allSlides[currSlide])
    .removeClass('inactive')
    .addClass('active');

  // Update

  $(document).on('keydown', function (event) {
    var nextSlide = currSlide;

    if (event.keyCode === 37) { // clicked left arrow
      console.log('previous slide');
      nextSlide = currSlide ? currSlide - 1 : currSlide;
    } else if (event.keyCode === 39) { // clicked right arrow
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
