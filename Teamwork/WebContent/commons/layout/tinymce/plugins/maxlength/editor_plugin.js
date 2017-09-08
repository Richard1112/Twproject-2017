(function() {
    tinymce.create('tinymce.plugins.TinyMCE_Maxlength', {
        _maxlength_onchange_callback : false,
        _maxlength_value : false,
        _maxlength_lastContent : '',
        _id : '',

        init : function(ed, url) {
            var plugin = tinymce.plugins.TinyMCE_Maxlength;
            // if you want to add a CallBack method.
            plugin._maxlength_onchange_callback=ed.getParam("maxlength_onchange_callback", false);

            ed.onNodeChange.add(
                function(ed, cm, n) {
                    //plugin._id = ed.id;
                   //plugin.prototype._maxLength(ed);
                }
            );

            ed.onChange.add(
                function(ed, l) {
                    plugin.prototype._maxLength(ed);
                    ed.setContent(ed.getContent().substring(0, plugin.prototype._getMaxLength(ed)));
                }
            );

            ed.onClick.add(
                function(ed, l) {
                  var max = plugin.prototype._getMaxLength(ed);
                  var conte = ed.getContent();
                  if(max && conte.length>max) {
                    plugin.prototype._maxLength(ed);
                    ed.setContent(conte.substring(0, max));
                  }
                }
            );

//            ed.onEvent.add(
//                function(ed, e) {
//                  //alert(e.type);
//                  var max = plugin.prototype._getMaxLength(ed);
//                  var conte = ed.getContent();
//                  if( (e.type=='keyup' ) && max && conte.length==max) {
//                    plugin.prototype._maxLength(ed);
//                    ed.setContent(conte.substring(0, max));
//                  }
//                }
//            );
        },

        _maxLength : function(ed) {
            plugin = tinymce.plugins.TinyMCE_Maxlength;
            max_length = plugin.prototype._getMaxLength(ed);
            var content=ed.getContent();
            var len=plugin.prototype._stripHtmlTags(content).length;
            if(len>max_length){
                //ed.setContent(plugin._maxlength_lastContent);
            } else {
                plugin._maxlength_lastContent=content;
            }
            if(plugin._maxlength_onchange_callback)
                eval(plugin._maxlength_onchange_callback+'(len, max_length);');
        },

        _getMaxLength : function(ed) {
            return ed.getParam("maxlength_"+ed.id, false);
        },

        _stripHtmlTags : function(strContent) {
            return strContent.replace(/(<([^>]+)>)/ig, "");
        }
    });
    // Register plugin
    tinymce.PluginManager.add('maxlength', tinymce.plugins.TinyMCE_Maxlength);
})();