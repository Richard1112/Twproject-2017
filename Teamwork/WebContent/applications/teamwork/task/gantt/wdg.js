function ld(id){
  var iframe=document.getElementById(id);
  var w=iframe.width;

  var h=iframe.height;
  //console.debug(w,h);

  if (w<1024){
    var originalFactor=h/w;
    var scale=w/1024;

    iframe.width=1024;
    iframe.height=1024*originalFactor;
    iframe.style.transform="scale("+scale+")";
    iframe.style.webkitTransform ="scale("+scale+")";
    iframe.style.marginLeft=-(1024-w)/2;
    iframe.style.marginRight=-(1024-w)/2;
    iframe.style.marginTop=-(iframe.height-h)/2;
    iframe.style.marginBottom=-(iframe.height-h)/2;
  }
  iframe.style.display="block";
}