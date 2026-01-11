openDic = function (id) {
  copyTextToClipboard(id);
  var text = document.getElementById(id).innerHTML;
  // let result = text.replace(/ /g, "+");
  var chaine = "https://www.boltidictionary.com/en/search?s=" + text;
  window.open(chaine);
};
