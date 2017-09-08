/**
 * $Id: editor_plugin_src.js 520 2008-01-07 16:30:32Z spocke $
 *
 * @author OpenLab
 * @copyright Copyright 2004-2008, OpenLab, All rights reserved.
 */
(function() {
	tinymce.create('tinymce.plugins.MediaEmbedderPlugin', {
		init : function(ed, url) {
			// Register commands
			ed.addCommand('mceMediaEmbedder', function() {
			  if (typeof objectId =='undefined')
			    objectId = '';
			  if (typeof objectClass =='undefined')
			    objectClass = '';

				ed.windowManager.open({
				  //file : url + '/mediaEmbedder.jsp?objectId='+objectId+'&objectClass='+objectClass + '&ts='+new Date().getMilliseconds(),
				  file : url + '/mediaEmbedder.jsp',
					width : 700 + parseInt(ed.getLang('mediaEmbedder.delta_width', 0)),
					height : 200 + parseInt(ed.getLang('mediaEmbedder.delta_height', 0)),
					inline : 1
				}, {
					plugin_url : url, // Plugin absolute URL
					some_custom_arg : 'custom arg' // Custom argument
				});
			});
			// Register buttons
			ed.addButton('mediaEmbedder', {title : 'Media Embedder', cmd : 'mceMediaEmbedder', image : url + '/images/mediaEmbedder.gif'});
		},
		getInfo : function() {
			return {
				longname : 'MediaEmbedder plugin',
				author : 'OpenLab',
				authorurl : 'http://www.open-lab.com',
				version : "1.0"
			};
		}
	});
	// Register plugin
	tinymce.PluginManager.add('mediaEmbedder', tinymce.plugins.MediaEmbedderPlugin);
})();