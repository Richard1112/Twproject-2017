/*
 Copyright (c) 2012-2014 Open Lab
 Written by Roberto Bicchierai http://roberto.open-lab.com
 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
function glanceIt(movementTarget, images, drawerCallback) {
  //console.debug("glanceIt",target,images,drawerCallback)

  //prepare vertex array and store it on images in order to speed-up
  images.each(function () {
    var img = $(this);
    var imgX = img.position().left + img.width() / 2;
    var imgY = img.position().top + img.height() / 2;

    var size = Math.sqrt(Math.pow(img.width(), 2) + Math.pow(img.height(), 2)) / 2; //raggio ideale dell'immagine

    //compute vertexes
    var vertex = [
      [imgX, imgY], //centro
      [imgX, imgY - size], //n
      [imgX + size, imgY - size], //ne
      [imgX + size, imgY], // e
      [imgX + size, imgY + size], //se
      [imgX, imgY + size], //s
      [imgX - size, imgY + size], //so
      [imgX - size, imgY], //o
      [imgX - size, imgY - size], //no
    ];
    img.data("glancedVertex", vertex);
  });

  //store vertex on image
  movementTarget.data("glancedImages", images);

  //array for traslate index to positions
  var compass = ["C", "N", "NE", "E", "SE", "S", "SW", "W", "NW"];


  //bind mouse move on target
  movementTarget.mousemove(function (ev) {
    var tg = $(this);
    var mouseX = ev.pageX;
    var mouseY = ev.pageY;
    tg.data("glancedImages").each(function () {
      var img = $(this);
      var vertex = img.data("glancedVertex");
      var dist = [];
      for (var i = 0; i < vertex.length; i++) {
        //compute distances to vertex
        var p = vertex[i];
        dist.push(Math.sqrt(Math.pow(mouseX - p[0], 2) + Math.pow(mouseY - p[1], 2)));
      }

      //get the inde of closer vertex
      var minIndex = 0;
      var minDist = 1e999;
      for (var i = 0; i < dist.length; i++) {
        if (minDist > dist[i]) {
          minIndex = i;
          minDist = dist[i];
        }
      }
      drawerCallback(img, compass[minIndex]);
    });
  }).mouseleave(function () {
    var tg = $(this);
    tg.data("glancedImages").each(function () {
      drawerCallback($(this), compass[0]);
    })
  });

  //set all to center
  movementTarget.data("glancedImages").each(function () {
    drawerCallback($(this), compass[0]);
  });

}