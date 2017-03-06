//- Get all the sections on the current web page.
//+ sections :: forall e. Eff (dom :: DOM | e) (Array HTML)
exports.sections = function () {
  var collection = document.getElementsByTagName('section');

  return Array.apply(null, collection);
};

//- Change the display property of a given element.
//+ display :: forall e. String -> HTML -> Eff (dom :: DOM | e) Unit
exports.display = function (display) {
  return function (element) {
    return function () {
      element.style.display = display;

      return {};
    };
  };
};
