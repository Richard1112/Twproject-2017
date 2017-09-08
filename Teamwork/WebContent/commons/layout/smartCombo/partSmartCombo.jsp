<%@ page import="org.jblooming.utilities.JSP,
                org.jblooming.utilities.StringUtilities,
                org.jblooming.waf.SessionState,
                org.jblooming.waf.html.core.JspIncluderSupport,
                org.jblooming.waf.html.input.SmartCombo,
                org.jblooming.waf.html.input.TextField,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageState,
                java.util.List"%><%

  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.getSessionState();

  if (SmartCombo.DRAW_INPUT.equals(request.getAttribute(SmartCombo.ACTION))) {
  SmartCombo smartCombo = (SmartCombo) JspIncluderSupport.getCurrentInstance(request);

    /* ---- define letsubmit function only when needed-------- */

    if (JSP.ex(smartCombo.onValueSelectedScript)){
      %><script>
      function letSubmit<%=smartCombo.fieldName%>() {
        <%=JSP.w(smartCombo.onValueSelectedScript)%>
      }
    </script><%
  }

    /* --------------------------------------- START TEXT FIELD PART ----------------------------------------------------------------- */

    sessionState.getAttributes().put(smartCombo.fieldName, smartCombo);
    // HIDDEN

  // 4/8/08 modifica da readonly=true a false  fatta da Matteo Bicocchi per problemi sul back e settaggio di cliententry su campi hidden
    TextField tfh = new TextField("hidden", "", smartCombo.fieldName, "", 10, false);
    if (smartCombo.required && !smartCombo.addAllowed) { // only if ! addAllowed the hidden part is required, otherwise is required the visible one
        tfh.required = true;
    }
    tfh.preserveOldValue = smartCombo.preserveOldValue;
    tfh.script=" jspPart='partSmartCombo.jsp'";

    String id = pageState.getEntry(smartCombo.fieldName ).stringValueNullIfEmpty();

    // get the the value from id
    // if not already there, inorder to preserve strange values from JST added on 17/12/2014 robicch
    if (!JSP.ex(pageState.getEntry(smartCombo.fieldName + SmartCombo.TEXT_FIELD_POSTFIX)))
      pageState.addClientEntry(smartCombo.fieldName + SmartCombo.TEXT_FIELD_POSTFIX, smartCombo.getTextValue(pageState));

    // VISIBLE
    TextField tf = new TextField("text", smartCombo.label, smartCombo.fieldName + SmartCombo.TEXT_FIELD_POSTFIX, smartCombo.separator, smartCombo.fieldSize, smartCombo.disabled);
    tf.required = smartCombo.required; // only if add allowed the visible part is required, otherwise is required the hidden part
    tf.readOnly = smartCombo.readOnly;
    tf.disabled = smartCombo.disabled;
    tf.fieldClass = smartCombo.fieldClass;
    tf.label = smartCombo.label;
    if(!JSP.ex(smartCombo.label))
      tf.showLabel = false;
    tf.innerLabel=smartCombo.innerLabel;

    tf.preserveOldValue = false;

    //if (!smartCombo.disabled && !smartCombo.readOnly) {
      StringBuffer script = new StringBuffer();
      script.append(" autocomplete=\"off\" onfocus=\"createDropDown($(this),"+smartCombo.iframe_width+","+smartCombo.iframe_height+"); refreshDropDown ($(this).nextAll('.cbDropDown'),$(this)); setSelection(this,0,1024)\" ");
      script.append("onblur=\"finalizeOperation($(this).nextAll('.cbDropDown:first')," + smartCombo.required +","+ smartCombo.addAllowed + " );" +
          (smartCombo.onBlurAdditionalScript != null && smartCombo.onBlurAdditionalScript.trim().length() > 0 ? smartCombo.onBlurAdditionalScript : "") + "\"");
      if (smartCombo.script != null && smartCombo.script.trim().length() > 0)
        script.append(" "+smartCombo.script);

      // test 17/07/2006 introduced onkeyPress to prevent the submission of a form with 1 only sc.
      //script.append(" onKeyUp=\"manageKeyEvent ($(this),event," + smartCombo.required +","+ smartCombo.addAllowed + ");\" onKeyPress=\"stopKeyEvent(event);\"");

      //17/06/2013 bicch & chela changed to keyDown in order to avoid Chrome to give focus on next element before completing operations
      script.append(" onKeyDown=\"manageKeyEvent ($(this),event," + smartCombo.required +","+ smartCombo.addAllowed + ");\" onKeyPress=\"stopKeyEvent(event);\"");
      tf.script = script.toString();
    //}

    String spanScript="";

    //if (!(smartCombo.disabled || smartCombo.readOnly)) {
      spanScript= " style='cursor:pointer; margin-left: -15px' onClick=\"if ( $(this).prevAll('.cbDropDown:first').size()<=0) {$(this).prevAll('input:text:first').focus();} \"" ;
    //}

    tf.toHtml(pageContext);
    //open.toHtml(pageContext);
  %><span class="teamworkIcon menuArrow" <%=spanScript%> style="">&ugrave;</span><%
    tfh.toHtml(pageContext);  //DO NOT CHANGE ORDER hidden is the last one in order to be easy to find it via jquery

    if (smartCombo.linkToEntity!=null) {
      if (JSP.ex(id)) {
        %><span id="<%=smartCombo.fieldName + SmartCombo.LINKENTITY_POSTFIX%>" class="<%=SmartCombo.LINKENTITY_POSTFIX%> "><%
        smartCombo.linkToEntity.setMainObjectId(id);
        smartCombo.linkToEntity.toHtmlInTextOnlyModality(pageContext);
        %></span><%
      }
    }

  } else {
    /* --------------------------------------- START DROP DOWN PART ----------------------------------------------------------------- */

    SmartCombo smartCombo = (SmartCombo) sessionState.getAttributes().get(request.getParameter("id"));
    if (smartCombo == null) {
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

      if (smartCombo.convertToUpper)
        filter = filter.toUpperCase();

      // fill the list with the filter
      String hiddenValue=pageState.getEntry("hiddenValue").stringValueNullIfEmpty();
      List<Object[]> prs = smartCombo.fillResultList(filter,hiddenValue);

      %><table width="100%" border="0" class="comboTable <%=JSP.w(smartCombo.dropDownFieldClass)%>" style="cursor:pointer"><%
      if (prs != null && prs.size() > 0) {

        int row=1;
        for (Object[] value : prs) {
          String res1 = value[smartCombo.columnToCopyInDescription] + "";
          %><tr class="trNormal <%=smartCombo.highlightedIds.contains(value[0])?"trHl":""%> <%=JSP.ex(hiddenValue)&&row==1?"trSel":""%> scTr" id="ROW_<%=row%>" selectText="<%=JSP.htmlEncodeTag(JSP.htmlEncodeApexes(res1.trim()))%>" selectValue="<%=JSP.javascriptEncode(JSP.w(value[0]))%>"><%
          for (int i = 1; i < value.length; i++) {
            %><td class="<%=i>1?"textSmall":""%>"><%=JSP.cleanHTML(JSP.w(value[i]))%></td><%
          }
          %></tr><%
          row++;
        }

        if (row==2) {
          %><tr class="unselectable" style="cursor:default"><td colspan="90"><small><i>(<%=I18n.get("RESULT_EXACTLY_ONE_HELP")%>)</i></small></td></tr><%
        }

        //this may get it wrong if prs.size() is exactly the total number foundable, but piuommen its ok
        if (smartCombo.maxRowToFetch<=prs.size()) {
          %><tr class="unselectable" style="cursor:default"><td colspan="90"><i>...<%=I18n.get("RESULT_LIMITED_TO_%%",smartCombo.maxRowToFetch+"")%></i></td></tr><%
        }
      }
      //link to add entity if any
      if(smartCombo.addEntityButton!=null && !JSP.ex(hiddenValue) ){
        %><tr class="unselectable" class="addEntityLine"><td colspan="90"><%smartCombo.addEntityButton.toHtmlInTextOnlyModality(pageContext);%></td></tr><%
      } else if(!JSP.ex(prs)) {
        %><tr class="unselectable" style="cursor:default"><td colspan="90"><i><%=I18n.get("NO_ELEMENTS_FOUND")%></i></td></tr><%
      }
      %></table><%

    }

  }
%>
