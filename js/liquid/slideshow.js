/******************************************************************************/
/************** Setting Up SlideShow ******************************************/
/******************************************************************************/

var allSlides = $('.slide');
var currSlide = 0;

/* gotoSlide :: (Int) => void */
function gotoSlide(nextSlide){
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

$(function () {

  // Initialize: Hide all
  $('.slide').removeClass('active').addClass('inactive');
  $(allSlides[currSlide]).removeClass('inactive').addClass('active');

  // Update
  $('.prevbutton').click(function (event) {
    var nextSlide = currSlide;
    console.log('prev slide click');
    nextSlide = currSlide ? currSlide - 1 : currSlide;
    gotoSlide(nextSlide);
    event.preventDefault();
   });

  $('.nextbutton').click(function (event) {
    var nextSlide = currSlide;
    console.log('next slide click');
    nextSlide = currSlide < allSlides.length - 1 ? currSlide + 1 : currSlide;
    gotoSlide(nextSlide);
    event.preventDefault();
   });

});

/* progPaneSlide :: (Int) => Int */
function progPaneSlide(paneId){
  var paneId = "#program-pane-" + paneId;
  var elem   = $(paneId).closest(".slide");
  return allSlides.index(elem);

/*
var paneId = "#program-pane-1";
var elem   = $(paneId).closest(".slide");
var pos    = allSlides.index(elem);
 */
}


