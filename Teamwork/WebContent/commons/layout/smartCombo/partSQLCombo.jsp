<%@ page import="org.jblooming.utilities.JSP,
                org.jblooming.utilities.StringUtilities,
                org.jblooming.waf.SessionState,
                org.jblooming.waf.html.core.JspIncluderSupport,
                org.jblooming.waf.html.input.SQLCombo,
                org.jblooming.waf.html.input.TextField,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageState,
                java.util.List"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.getSessionState();

 if (SQLCombo.DRAW_INPUT.equals(request.getAttribute(SQLCombo.ACTION))) {
    SQLCombo sqlCombo = (SQLCombo) JspIncluderSupport.getCurrentInstance(request);

    /* ---- define letsubmit function only when needed-------- */

    if (JSP.ex(sqlCombo.onValueSelectedScript)){
      %><script>
      function letSubmit<%=sqlCombo.fieldName%>() {
        <%=JSP.w(sqlCombo.onValueSelectedScript)%>
      }
    </script><%
  }

    /* --------------------------------------- START TEXT FIELD PART ----------------------------------------------------------------- */

    sessionState.getAttributes().put(sqlCombo.fieldName, sqlCombo);
    // HIDDEN

    TextField tfh = new TextField("hidden", "", sqlCombo.fieldName, "", 10, false);
    if (sqlCombo.required ) {
        tfh.required = true;
    }
    tfh.preserveOldValue = sqlCombo.preserveOldValue;
    tfh.script=" jspPart='partSQLCombo.jsp'";


    // get the the value from id
  // if not already there, inorder to preserve strange values from JST added on 17/12/2014 robicch
    if (!JSP.ex(pageState.getEntry(sqlCombo.fieldName + sqlCombo.TEXT_FIELD_POSTFIX)))
      pageState.addClientEntry(sqlCombo.fieldName + SQLCombo.TEXT_FIELD_POSTFIX, sqlCombo.getTextValue(pageState));



    // VISIBLE
    TextField tf = new TextField("text", sqlCombo.label, sqlCombo.fieldName + sqlCombo.TEXT_FIELD_POSTFIX, sqlCombo.separator, sqlCombo.fieldSize, sqlCombo.disabled);
    tf.required = sqlCombo.required; // only if add allowed the visible part is required, otherwise is required the hidden part
    tf.readOnly = sqlCombo.readOnly;
    tf.disabled = sqlCombo.disabled;
    tf.fieldClass = sqlCombo.fieldClass;
    tf.label = sqlCombo.label+(sqlCombo.required?"*":"");
    tf.innerLabel=sqlCombo.innerLabel;

    tf.preserveOldValue = false;

    if (!sqlCombo.disabled && !sqlCombo.readOnly) {
      StringBuffer script = new StringBuffer();
      script.append(" autocomplete=\"off\" onfocus=\"createDropDown($(this),"+sqlCombo.iframe_width+","+sqlCombo.iframe_height+"); refreshDropDown ($(this).nextAll('.cbDropDown'),$(this)); setSelection(this,0,1024)\" ");
      script.append("onblur=\"finalizeOperation($(this).nextAll('.cbDropDown:first')," + sqlCombo.required +",false );" +
          (sqlCombo.onBlurAdditionalScript != null && sqlCombo.onBlurAdditionalScript.trim().length() > 0 ? sqlCombo.onBlurAdditionalScript : "") + "\"");
      if (sqlCombo.script != null && sqlCombo.script.trim().length() > 0)
        script.append(" "+sqlCombo.script);

      //17/06/2013 bicch & chela changed to keyDown in order to avoid Chrome to give focus on next element before completing operations
      //script.append(" onKeyUp=\"manageKeyEvent ($(this),event," + sqlCombo.required +",false);\" onKeyPress=\"stopKeyEvent(event);\"");
      script.append(" onKeyDown=\"manageKeyEvent ($(this),event," + sqlCombo.required +",false);\" onKeyPress=\"stopKeyEvent(event);\"");

      tf.script = script.toString();
    }

    String spanScript="";

    if (!(sqlCombo.disabled || sqlCombo.readOnly)) {
      spanScript= " style='cursor:pointer; margin-left: -15px' onClick=\"if ( $(this).prevAll('.cbDropDown:first').size()<=0) {$(this).prevAll('input:text:first').focus();} \"" ;
    }

    tf.toHtml(pageContext);
    //open.toHtml(pageContext);
  %><span class="teamworkIcon menuArrow" <%=spanScript%> style="">&ugrave;</span><%
    tfh.toHtml(pageContext);  //DO NOT CHANGE ORDER hidden is the last one in order to be easy to find it via jquery

  } else {
    /* --------------------------------------- START DROP DOWN PART ----------------------------------------------------------------- */

    SQLCombo SQLCombo = (SQLCombo) sessionState.getAttributes().get(request.getParameter("id"));
    if (SQLCombo == null) {
      //here probably the sesssion is dead and there is no combo, show a error message
      %><%=I18n.get("SESSION_EXPIRED_REFRESH_PAGE")%><%

    } else {
      String filter = request.getParameter("filter");
      if (filter!=null)
        filter = StringUtilities.replaceAllNoRegex(filter, "\\", "\\\\");
      if (filter == null)
        filter = "";


      if (filter.length() > 0) {
        // remove last char if backspace
        if ("8".equals(request.getParameter("key"))) {
            filter = filter.substring(0, filter.length() - 1);
        }
      }

      if (SQLCombo.convertToUpper)
        filter = filter.toUpperCase();

      // fill the list with the filter
      String hiddenValue=pageState.getEntry("hiddenValue").stringValueNullIfEmpty();
      List<Object[]> prs = SQLCombo.fillResultList(filter,hiddenValue);

      if (prs != null) {

        if (prs.size() > 0) {

          %><table width="100%" border="0" class="comboTable <%=JSP.w(SQLCombo.dropDownFieldClass)%>" style="cursor:pointer"><%
          int row=1;
          for (Object[] value : prs) {
            String res1 = value[SQLCombo.columnToCopyInDescription] + "";
            %><tr class="trNormal <%=SQLCombo.highlightedIds.contains(value[0])?"trHl":""%> scTr"
                  id="ROW_<%=row%>"
                  selectText="<%=JSP.htmlEncodeTag(JSP.htmlEncodeApexes(res1.trim()))%>"
                  selectValue="<%=JSP.javascriptEncode(JSP.w(value[0]))%>">
            <%
            for (int i = 1; i < value.length; i++) {
              %><td><%=JSP.cleanHTML(JSP.w(value[i]))%>&nbsp;</td><%
            }
            row++;
          }

          if (row==2) {
            %><tr class="unselectable" style="cursor:default"><td colspan="90"><small><i>(<%=I18n.get("RESULT_EXACTLY_ONE_HELP")%>)</i></small></td></tr><%
          }

          //this may get it wrong if prs.size() is exactly the total number foundable, but piuommen its ok
          if (SQLCombo.maxRowToFetch<=prs.size()) {
            %><tr class="unselectable" style="cursor:default"><td colspan="90"><i>...<%=I18n.get("RESULT_LIMITED_TO_%%",SQLCombo.maxRowToFetch+"")%></i></td></tr><%
          }
          %></table><%
   
        }
      }
    }
  }
%>
