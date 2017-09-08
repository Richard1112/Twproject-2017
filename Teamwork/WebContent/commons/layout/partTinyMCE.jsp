<%@ page import="org.jblooming.operator.Operator,
                 org.jblooming.utilities.HtmlSanitizer,
                 org.jblooming.utilities.JSP,
                 org.jblooming.waf.SessionState,
                 org.jblooming.waf.html.core.JspIncluderSupport,
                 org.jblooming.waf.html.input.TinyMCE,
                 org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState" %><%@ page pageEncoding="UTF-8" %><%

    PageState pageState = PageState.getCurrentPageState(request);
    SessionState sessionState = pageState.sessionState;
    Operator logged = pageState.getLoggedOperator();

    TinyMCE mce = (TinyMCE)JspIncluderSupport.getCurrentInstance(request);

    if(TinyMCE.DISABLE.equals(request.getAttribute(TinyMCE.ACTION))) {
      %><div><%=HtmlSanitizer.sanitize(pageState.getEntry(mce.fieldName).stringValueNullIfEmpty())%></div><%

    } else {
      String relPath = (JSP.ex(ApplicationState.getApplicationSetting("SITE_FOLDER_LOCATION")) ? ApplicationState.getApplicationSetting("SITE_FOLDER_LOCATION") : "") + mce.sitePrefix;

      if (TinyMCE.INITIALIZE.equals(request.getAttribute(TinyMCE.ACTION))) {
        %><script type="text/javascript">
            var relPath = "<%=relPath%>";
            function alertOnMaxLen (len, maxlength) {
               if(maxlength && len>maxlength)
                 alert('<%=pageState.getI18n("MAX_LENGTH_EXCEEDED")%>: ' + maxlength);
            }

            function _tinySetupFunctions(ed){
             ed.onSaveContent.add(function(ed, o) {
                     ed.startContent= tinymce.trim(ed.getContent({format : 'raw', no_events : 1}));
                   });
              <%if (mce.useTinyCustomSetupJSFunction){%>
                tinyCustomSetup(ed);
              <%}%>
           }
          </script><%

        // in case of a ajax list script MUST be included in page header only once 
        if(mce.includeScript) {
          %><script language="javascript" type="text/javascript" src="<%=request.getContextPath()%>/commons/layout/tinymce/tiny_mce_src.js"></script><%
        }

        mce.baseUrl = mce.getApplicationBaseURL(request);

      } else if (TinyMCE.DRAW.equals(request.getAttribute(TinyMCE.ACTION))) {
        if (mce.readOnly) {
          mce.textArea.readOnly = true;
          mce.textArea.toHtml(pageContext);

        } else {
          // debug
          if(mce.limitTextSize) {
            mce.addParameter("maxlength_"+mce.id,mce.textArea.maxlength+"");
            mce.addParameter("maxlength_onchange_callback","alertOnMaxLen");
            mce.additionalPlugins.add("maxlength");
          }

          if (mce.required) {
            mce.textArea.required = true;
          }
          mce.textArea.script = mce.textArea.script + " maleficoTiny='" + mce.textArea.id + "' ";

          mce.addJSParameter("setup", "_tinySetupFunctions");

          // convert fixed attributes to parameters
          mce.addParameter("tmce_ta_id",mce.textArea.id);
          mce.addParameter("mode",TinyMCE.MODE_EXACT);
          mce.addParameter("elements",mce.fieldName);
          mce.addParameter("document_base_url",mce.baseUrl);

          if(mce.resize) {
            mce.addJSParameter("theme_advanced_resizing","true");

            // NB: do not use for TinyMCE.THEME_ADVANCED!
            if(TinyMCE.THEME_SIMPLE.equalsIgnoreCase(mce.getTheme()))
              mce.addParameter("theme_advanced_statusbar_location", "bottom");

            mce.addJSParameter("theme_advanced_resize_horizontal","false");
          } else {
            mce.addJSParameter("theme_advanced_resizing", "false");
          }

          mce.addParameter("content_css",mce.content_css);

          if (mce.showHTMLButton)
            mce.addParameter("theme_advanced_buttons1", "code");

          if (JSP.ex(mce.height)) {
            mce.addParameter("height",mce.height+"px");
          }
          if (JSP.ex(mce.width)) {
            mce.addParameter("width",mce.width+"px");
          }

          mce.addParameter("imageSearchField",mce.imageSearchField);
          mce.addParameter("objectClass",mce.objectClass);

          if(!mce.forceNewLinesAsParagraph) {
            mce.addJSParameter("forced_root_block","false");
          }

          if (mce.relativeUrls) {
            mce.addJSParameter("relative_urls", "true");
            mce.addJSParameter("remove_script_host", "true");
          } else {
            mce.addJSParameter("relative_urls", "false");
            mce.addJSParameter("remove_script_host", "false");
          }

          String bar = mce.pluginBar;
          if(TinyMCE.THEME_SIMPLE.equals(mce.getTheme()))
            bar = "theme_advanced_buttons1";

          // plugins are added in theme_advanced_buttons2
          for(String adds : mce.additionalPlugins) {
            mce.addParameter("plugins",adds);
            mce.addParameter(bar,adds);
          }

        %><script language="javascript" type="text/javascript">
        tinyMCE.init( {
        <%
         // initParameters
         boolean first=true;
         for (String param :mce.initParameters.keySet()){
            %><%=(!first?",":"")+param%>:<%=mce.initParameters.get(param)%><%
            first=false;
         }       
      %>} );
      </script><%
        mce.textArea.toHtml(pageContext);
      }

    }

  }
%>