<%@ page import="org.jblooming.waf.SessionState,org.jblooming.waf.html.button.ButtonJS,org.jblooming.waf.html.input.InputElement,org.jblooming.waf.html.input.TextField,
                 org.jblooming.waf.html.state.Form,org.jblooming.waf.settings.ApplicationState,org.jblooming.waf.view.PageSeed,org.jblooming.waf.view.PageState"%><%@ page pageEncoding="UTF-8" %>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"><%

  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.sessionState;


%>
<html><head>
  <link rel=stylesheet href="<%=request.getContextPath()%>/commons/skin/deafult/platform.css.jsp?<%=ApplicationState.getBuild()%>" type="text/css" media="screen">
  <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3/jquery.min.js?<%=ApplicationState.getBuild()%>"></script>
  <script type="text/javascript" src="<%=request.getContextPath()+"/commons/js/jquery/jquery.livequery.min.js"%>?<%=ApplicationState.getBuild()%>"></script>
  <script type="text/javascript" src="<%=request.getContextPath()+"/commons/layout/tinymce/tiny_mce_popup.js"%>"></script>
  <script type="text/javascript" src="<%=request.getContextPath()%>/commons/layout/tinymce/plugins/mediaEmbedder/js/mediaEmbedder.js"></script>
  <script language="javascript" type="text/javascript" src="../../tiny_mce_popup.js"></script>
  <script type="text/javascript" src="<%=request.getContextPath()+"/commons/js/platform.js"%>?<%=ApplicationState.getBuild()%>"></script><script type="text/javascript">
      var serverURL = "<%=ApplicationState.serverURL%>";
      var contextPath = "<%=ApplicationState.contextPath%>";
      var inhibitTooltips = "true";
  </script>
	<base target="_self"/>
	</head><body><%

  PageSeed self = pageState.thisPage(request);
  Form form = new Form(self);
  pageState.setForm(form);
  form.start(pageContext);

%><base target="_self"/>
  <table class="table" border="0" width="100%" cellpadding="4">
    <tr><td colspan="3" style="font-size:12px;">Insert an absolute media path (for example http://www.youtube.com/watch?v=WD42qLEVssM)...</td></tr>
     <tr><td width="1%" nowrap="nowrap"><%

      TextField imgPath = new TextField("<b>Media path</b>", "MOV_PATH", "</td><td>", 40, false);
      imgPath.entryType = InputElement.EntryType.URL;
      imgPath.toHtml(pageContext);

    %></td><td width="*"><table width="100%" border="0"><tr>
    <td align="right"><span style="cursor:pointer"><%

    ButtonJS insert = new ButtonJS();
    insert.additionalCssClass="brown";
    insert.label = "<b>Insert</b>";
    insert.additionalOnClickScript = " var val=$('#MOV_PATH').val(); if(val && val.indexOf('http')!=-1) { MediaEmbedderDialog.insert( val ); } else { alert('Insert media path!') }";
    insert.toHtml(pageContext);

   %></span></td></tr></table></td></tr>
   <tr>
   <td colspan="2">YouTube, Vimeo, Ustream and Livestream are the only accepted video providers.</td>
   <td align="right"><span style="cursor:pointer;"><b><%

    // close
    ButtonJS close = new ButtonJS();
    close.additionalCssClass="brown";
    close.onClickScript= " tinyMCEPopup.close(); ";
    close.label = "Close";
    close.toHtml(pageContext);

    %></b></span></td></tr></table><%

    form.end(pageContext);

%></body></html>