tinyMCEPopup.requireLangPack();
var MediaEmbedderDialog = {
	init : function(ed) {
		tinyMCEPopup.resizeToInnerSize();
	},
	insert : function(str) {
		var ed = tinyMCEPopup.editor;
    if(str) {
      if(str.toLowerCase().indexOf('vimeo')!=-1)
        str = '[vimeo='+str+']';
      if(str.toLowerCase().indexOf('youtube')!=-1)
        str = '[youtube='+str+']';
      if(str.toLowerCase().indexOf('ustream')!=-1)
        str = '[ustream='+str+']';
      if(str.toLowerCase().indexOf('livestream')!=-1)
        str = '[livestream='+str+']';
      if(str.toLowerCase().endsWith('.mp3'))
        str = '[audio='+str+']';
        
      ed.execCommand('mceInsertContent', false, "<span>" + str + "</span>");

    } else {
      alert('Please insert an absolute url');
      return false;
    }

		tinyMCEPopup.close();
	}
};
tinyMCEPopup.onInit.add(MediaEmbedderDialog.init, MediaEmbedderDialog);