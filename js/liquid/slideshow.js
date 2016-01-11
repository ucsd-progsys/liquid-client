$(function () {

  var currSlide = 0;
  var allSlides = $('.slide');


  function updateSlides(nextSlide){
    if (nextSlide !== currSlide) {
        $(allSlides[currSlide])
          .removeClass('active')
          .addClass('inactive');
        $(allSlides[nextSlide])
          .removeClass('inactive')
          .addClass('active');

        currSlide = nextSlide;
    }
  }

  // Initialize: Hide all
  $('.slide').removeClass('active').addClass('inactive');
  $(allSlides[currSlide]).removeClass('inactive').addClass('active');

  // Update
  $('.prevbutton').click(function (event) {
    var nextSlide = currSlide;
    console.log('prev slide click');
    nextSlide = currSlide ? currSlide - 1 : currSlide;
    updateSlides(nextSlide);
    event.preventDefault();
   });

  $('.nextbutton').click(function (event) {
    var nextSlide = currSlide;
    console.log('next slide click');
    nextSlide = currSlide < allSlides.length - 1 ? currSlide + 1 : currSlide;
    updateSlides(nextSlide);
    event.preventDefault();
   });

 
  // https://css-tricks.com/snippets/javascript/javascript-keycodes/
  /*
  $(document).on('keydown', function (event) {
    // page up
    if (event.keyCode === 33) { 
      console.log('previous slide');
      nextSlide = currSlide ? currSlide - 1 : currSlide;
    // page down
    } else if (event.keyCode === 34) { 
      console.log('next slide');
      nextSlide = currSlide < allSlides.length - 1 ? currSlide + 1 : currSlide;
    }
  });
  */

});
