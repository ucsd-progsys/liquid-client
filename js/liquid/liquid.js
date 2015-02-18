'use strict';

function replicate(n, x) {
    var a = [];
    for (var i = 0; i < n; i++){
        a.push(x);
    }
    return a;
}

function replicatei(n, f) {
    var a = [];
    for (var i = 0; i < n; i++){
        a.push(f(i));
    }
    return a;
}


/*******************************************************************************/
/************** Setting Up Editor **********************************************/
/*******************************************************************************/

var SrcMode     = require(editorMode).Mode; // var SrcMode     = require("ace/mode/haskell").Mode;
var numEditors  = $('.welleditor').length;
var progEditor  = [];
var typeTooltip = [];

function programId(i)     { return "program-"      + i; }
function programPaneId(i) { return "program-pane-" + i; }

// Create Editors
for (var i = 0; i < numEditors; i++){
    var pi = ace.edit(programId(i));
    pi.renderer.setShowGutter(false)
    pi.setShowPrintMargin(false);
    pi.setOptions({ maxLines: Infinity});
    pi.setTheme(editorTheme);    // progEditor.setTheme("ace/theme/xcode");
    pi.setOptions({
        fontFamily: "Source Code Pro",
        fontSize: "13pt"
    });
    pi.getSession().setMode(new SrcMode());
    typeTooltip[i] = new TokenTooltip(pi, getAnnot(i));
    progEditor[i]  = pi; 
}

// lift `on` to work for collection of editors
progEditor.on = function(event, fn){
    for (var i = 0; i < numEditors; i++){
        progEditor[i].on(event, fn);
    }
}

// lift `setKeyboardHandler` to work for collection of editors
progEditor.setKeyboardHandler = function(h){
    for (var i = 0; i < numEditors; i++){
        progEditor[i].setKeyboardHandler(h);
    }
}

progEditor.getSourceBlocks = function (){
    return progEditor.map(function (p){ return p.getSession().getValue();});
}

progEditor.getSourceCode = function (){
    return progEditor.getSourceBlocks().join("\n");
}

// Globals
var progEditor.errorMarkers = replicate(numEditors, []); 


/*******************************************************************************/
/** Markers For Errors *********************************************************/
/*******************************************************************************/

function errorRange(err){
  
  var row0 = err.start.line - 1;
  var col0 = err.start.column - 1;
  var row1 = err.stop.line - 1;
  var col1 = err.stop.column - 1;
 
  if (row0 == row1 && col0 == col1){
    return new Range(row0, col0, row0, col0 + 1);
  } else {
    return new Range(row0, col0, row1, col1);
  }
}

function errorMarker(editor, err){
  var r = errorRange(err);
  return editor.session.addMarker(r, "ace_step", "error");
}

function errorAceAnnot(err){
    var etext = defaultErrText;
    if (err.message) {
        etext = err.message;
    }
    var ann = { row   : err.start.line - 1
              , column: err.start.column
              , text  : etext
              , type  : "error"
              };
    return ann;
}

function setErrorsOne(i, errs){
    var editor = progEditor[i];
    // Add Error Markers
    progEditor.errorMarkers[i].forEach(function(m){ editor.session.removeMarker(m); });
    progEditor.errorMarkers[i] = errs.map(function(e){ return errorMarker(editor, e);});
  
    // Add Gutter Annotations
    editor.session.clearAnnotations();
    var annotations  = errs.map(errorAceAnnot);
    editor.session.setAnnotations(annotations);
}

function setErrors(errs){
    for (var i = 0; i < numEditors; i++){
        var errsi = TODOTODO();
        setErrorsOne(i, errsi);
    } 
}

/*******************************************************************************/
/************** URLS ***********************************************************/
/*******************************************************************************/

function isPrefix(p, q) { 
  return (p == q.slice(0, p.length)) 
}

function getQueryURL(){ 
  return queryServerURL + 'query'; 
}

function getSrcURL(file){ 
  if (file.match("/")){
    return file;
  } else { 
    return ('demos/' + file);
  }
}



/*******************************************************************************/
/************** Queries ********************************************************/
/*******************************************************************************/

function getCheckQuery($scope){ 
  return { type    : "check",
           program : progEditor.getSourceCode() 
         };
}

function getRecheckQuery($scope){
  var p = "";
  if ($scope.filePath) p = $scope.filePath;

  return { type    : "recheck", 
           program : progEditor.getSourceCode(), 
           path    : p
         };
}

function getLoadQuery($scope){
  return { type    : "load", 
           path    : $scope.localFilePath
         };
}

function getSaveQuery($scope){
  return { type    : "save"
         , program : progEditor.getSourceCode()
         , path    : $scope.localFilePath
         };
}

function getPermaQuery($scope){
  return { type    : "perma"
         , program : progEditor.getSourceCode()
         };
}

/*******************************************************************************/
/************** Tracking Status and Source *************************************/
/*******************************************************************************/

function clearStatus($scope){
  $scope.isSafe       = false;
  $scope.isUnsafe     = false;
  $scope.isError      = false;
  $scope.isCrash      = false;
  $scope.isChecking   = false;
  $scope.isUnknown    = true ;
}

function setStatusChecking($scope){
  clearStatus($scope);
  $scope.isChecking = true;
  $scope.isUnknown  = false;
}

function setStatusResult($scope, data){
  var result          = getResult(data);
  debugResult         = result;
  clearStatus($scope);
  $scope.isChecking   = false;
  $scope.isSafe       = (result == "safe"  );
  $scope.isUnsafe     = (result == "unsafe");
  $scope.isCrash      = (result == "crash" );
  $scope.isError      = (result == "error" );
  $scope.isUnknown    = !($scope.isSafe || $scope.isError || $scope.isUnsafe || $scope.isCrash);
  $scope.filePath     = data.path;
  return result;
}

/*******************************************************************************/
/************** Loading Files **************************************************/
/*******************************************************************************/
// DEAD CODE. All loading happens via server.

/*@ fileText :: (file, (string) => void) => void */
function fileText(file, k){
  var reader = new FileReader();
  reader.addEventListener("load", function(e){
    k(e.target.result);
  });
  reader.readAsText(file);
}

/*******************************************************************************/
/** Extracting JSON Results ****************************************************/
/*******************************************************************************/

function getResult(d) { 
  var res = "crash";
  if (d) {
    res = d.status; 
  }
  return res;
}

function getWarns(d){ 
  var ws = [];
  if (d && d.errors){
    var ws = d.errors.map(function(x){ 
               return x.message;
             });
  }
  return ws;
}

/*******************************************************************************/
/************** Top-Level Demo Controller **************************************/
/*******************************************************************************/

var debugQuery  = null;
var debugData   = null;
var debugResult = null;
var debugResp   = 0;
var debugFiles  = null;
var debugDemo   = null;
var debugSrcURL = null;
var debugZ      = null;

function LiquidDemoCtrl($scope, $http, $location) {

  // Start in non-fullscreen
  // NUKE $scope.isFullScreen  = false; 
  // NUKE $scope.embiggen      = "FullScreen";
  // NUKE $scope.demoTitle     = demoTitle;
  // NUKE $scope.demoSubtitle  = demoSubtitle;
  // NUKE $scope.links         = allLinks;
  // NUKE $scope.categories    = getCategories();
  // NUKE $scope.isLocalServer = (document.location.hostname == "localhost");
  // NUKE $scope.localFilePath = "";

  clearStatus($scope);

  // For debugging
  $scope.gong =  function(s) { alert(s); };

  // Clear Status when editor is changed
  progEditor.on("change", function(e){ 
    $scope.$apply(function(){
      clearStatus($scope);
    });
  });
 
  // Change editor keybindings
  $scope.keyBindingsNone  = function (){ progEditor.setKeyboardHandler(null); };
  $scope.keyBindingsVim   = function (){ progEditor.setKeyboardHandler("ace/keyboard/vim"); };
  $scope.keyBindingsEmacs = function (){ progEditor.setKeyboardHandler("ace/keyboard/emacs"); };
  
  // http://www.cleverweb.nl/javascript/a-simple-search-with-angularjs-and-php/
  function verifyQuery(query){ 
    debugQuery = query;
    setStatusChecking($scope);
    $http.post(getQueryURL(), query)
         .success(function(data, status) {
            debugResp        = debugResp + 1; 
            $scope.status    = status;
            debugData        = data;
            $scope.warns     = getWarns(data); 
            $scope.annotHtml = data.annotHtml;
            $scope.result    = setStatusResult($scope, data);
           
            // This may be "null" if liquid crashed...
            if (data) { 
                setAnnots(progEditor.getSourceBlocks(), data.types);

              // TODO
              setErrors(data.errors);
            };
            
        })
         .error(function(data, status) {
            var msg = (data || "Request failed") + status;
            alert(msg);
         });
  };
  
  $scope.verifySource   = function(){ verifyQuery(getCheckQuery($scope));   };

  // $scope.reVerifySource = function(){ verifyQuery(getRecheckQuery($scope)); };
}

/************************************************************************/
/***** Initialize Angular ***********************************************/
/************************************************************************/

var demo = angular.module("liquidDemo", []);
demo.controller('LiquidDemoCtrl', LiquidDemoCtrl);
// toggleEditorSize({isFullScreen : false });
