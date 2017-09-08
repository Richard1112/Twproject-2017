<%@ page import=" com.twproject.messaging.stickyNote.StickyNote, com.twproject.messaging.stickyNote.StickyNoteDrawer, com.twproject.operator.TeamworkOperator, com.twproject.resource.Person,
                  org.jblooming.oql.OqlQuery, org.jblooming.tracer.Tracer, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenBasic, org.jblooming.waf.SessionState,
                  org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.core.JspIncluderSupport, org.jblooming.waf.html.display.HeaderFooter, org.jblooming.waf.settings.I18n,
                  org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.util.List"
    %><%
  PageState pageState = PageState.getCurrentPageState(request);
  SessionState sessionState = pageState.getSessionState();

  ScreenBasic screen = (ScreenBasic) JspIncluderSupport.getCurrentInstance(request);
  HeaderFooter hf = pageState.getHeaderFooter();
  hf.toolTip = "Teamwork";

  //added by robik to avoid loading of main pages following links on iframe or popup
  hf.keepAsTop=!pageState.isPopup();

  try {
    if (pageState.href.indexOf(".")>-1) {
      String name = pageState.href.substring(pageState.href.lastIndexOf("/") + 1, pageState.href.lastIndexOf("."));
      String histName = I18n.get("HIS_" + name.toUpperCase());
      if (!histName.startsWith("HIS_"))
        hf.toolTip = "Twproject - "+histName;
    }
  } catch (Throwable e) {
    Tracer.platformLogger.error(e);
  }
  hf.header(pageContext);

%>

<div id="twMainContainer" style="<%=pageState.isPopup()?"width:100%;":""%>">
  <div id="savingMessage" ><%=I18n.get("SAVING_MESSAGE")%></div>
    <%
  TeamworkOperator op = (TeamworkOperator)pageState.getLoggedOperator();

  if (screen.menu!=null && !pageState.isPopup()) {
    screen.menu.toHtml(pageContext);
  }
  %><div id="__FEEDBACKMESSAGEPLACE" style="display:none;"></div>
  <div id="twInnerContainer" class="<%=screen.getBody().areaHtmlClass%>"> <%
    screen.getBody().toHtml(pageContext);
  %></div><%
  TeamworkOperator teamworkOperator = (TeamworkOperator) pageState.getLoggedOperator();

  // get loggedUser sticky
  if (!pageState.isPopup() && teamworkOperator!=null){
    OqlQuery oql= new OqlQuery("from "+StickyNote.class.getName()+" as sticky where sticky.receiver=:rec and sticky.board is null");
    oql.getQuery().setEntity("rec",teamworkOperator.getPerson());
    List<StickyNote> stickyNotes=oql.list();
    if(stickyNotes!= null && stickyNotes.size()>=30){
      String message = I18n.get("TOO_MANY_STICKYNOTES_%%",stickyNotes.size() + "")+"";
      String buttonCloseAll = "<button onclick=\"$.get('"+request.getContextPath()+"/applications/teamwork/sticky/partAjaxDeleteStickyNote.jsp','STICKY_CMD=delete&logged="+op.getId()+"',function(ret){$('#__FEEDBACKMESSAGEPLACE').find('.FFC_Global:first').fadeOut('slow');});\">"+I18n.get("CLOSE_ALL")+"</button>";
      String buttonDrawAll = "<button onclick=\"$.get('"+request.getContextPath()+"/applications/teamwork/sticky/partAjaxDeleteStickyNote.jsp','STICKY_CMD=draw&logged="+op.getId()+"',function(ret){$('body').append(ret);$('#__FEEDBACKMESSAGEPLACE').find('.FFC_Global:first').fadeOut('slow');});\">"+I18n.get("DRAW_ALL")+"</button>";
      pageState.addMessageInfo(message + buttonDrawAll+buttonCloseAll);

    }else if (JSP.ex(stickyNotes)){
      int x = 200;
      int y = 200;
      for (StickyNote sn: stickyNotes){
        if (sn.getX()==0) {
          x = x + 20;
          sn.setX(x);
        }
        if (sn.getY()==0) {
          y = y + 20;
          sn.setY(y);
        }
        StickyNoteDrawer snd = new StickyNoteDrawer(sn);
        snd.toHtml(pageContext);
      }
    %>
  <script type="text/javascript">
    $(function() {
      var firstClosed = false;
      var doNotAskAgain = false;

      $(".container.sticky").bind("hide", function(e) {
        if (firstClosed && !doNotAskAgain) {
          var firstStick = $(this);
          var sticks = $(".container.sticky");
          if (sticks.size() > 2) {
            doNotAskAgain = true;
            if (confirm("<%=I18n.get("CLOSE_ALL_CONTAINERS")%>")) {
              sticks.each(function() {
                var stick = $(this);
                if (stick.attr("status") != "ICONIZED")
                  if (stick.prop("id")!=firstStick.prop("id"))
                    $(this).trigger("hide");
              });
            }
          }
          firstClosed = false;

        } else {
          firstClosed = true;
        }

      });
    });
  </script><%

    }
  }


  if (teamworkOperator != null) {
    Integer user_has_an_email = (Integer) sessionState.getAttribute("USER_HAS_AN_EMAIL");
    if (user_has_an_email == null)
      user_has_an_email = 0;
    if (user_has_an_email < 6) {
      Person p = teamworkOperator.getPerson();
      if (!JSP.ex(p.getDefaultEmail())) {
        user_has_an_email++;
        if (user_has_an_email == 5)
          user_has_an_email = 0;
        if (user_has_an_email == 2) {
          PageSeed e = pageState.pageFromRoot("resource/resourceEditor.jsp");
          e.setCommand(Commands.EDIT);
          e.addClientEntry("focusToEmail", true);
          e.setMainObjectId(p.getId());
          String wne = I18n.get("WARNING_NO_EMAIL");
          pageState.addMessageWarning(new ButtonLink(wne, e).toLink());
        }
        sessionState.setAttribute("USER_HAS_AN_EMAIL", user_has_an_email);
      } else {
        sessionState.setAttribute("USER_HAS_AN_EMAIL", 5);
      }
    }
  }


%></div><%
    //here custom meessages
    pageState.getHeaderFooter().footer(pageContext);

%>
