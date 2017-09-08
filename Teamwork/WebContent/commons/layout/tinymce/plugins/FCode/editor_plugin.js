/**
 * $Id: editor_plugin_src.js 520 2008-01-07 16:30:32Z spocke $
 *
 * @author Matteo Bicocchi
 * @copyright Copyright © 2001-2009, Matteo Bicocchi, Open lab, Firenze.
 */

(function() {
	tinymce.create('tinymce.plugins.FCode', {
		init : function(ed,url) {
			// Register commands
			ed.addCommand('mceFCode', function() {
        var sel = ed.selection.getContent();
                sel = '<pre class="-ln"><code class="js">' + sel + '&nbsp;</code></pre>';
                ed.selection.setContent(sel);
			});

      // Register button
      ed.addButton('FCode', {
        title : 'format as code',
        cmd : 'mceFCode',
        image : url + '/img/FCode.gif'
      });
		},

		getInfo : function() {
			return {
				longname : 'Format as Code',
				author : 'Matteo Bicocchi, Open lab srl',
				authorurl : 'http://pupunzi.com',
				infourl : '',
				version : tinymce.majorVersion + "." + tinymce.minorVersion
			};
		}
	});

	// Register plugin
	tinymce.PluginManager.add('FCode', tinymce.plugins.FCode);
})();