<%@ page import="com.twproject.task.Issue, com.twproject.task.businessLogic.IssueController,
com.twproject.waf.TeamworkPopUpScreen, org.jblooming.page.Page, org.jblooming.system.SystemConstants,
org.jblooming.utilities.DateUtilities, org.jblooming.utilities.JSP, org.jblooming.waf.ScreenArea, org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields,
org.jblooming.waf.html.button.ButtonJS, org.jblooming.waf.html.button.ButtonSubmit, org.jblooming.waf.html.container.Container, org.jblooming.waf.html.display.Img, org.jblooming.waf.html.input.CheckField, org.jblooming.waf.html.state.Form,
org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);


  if (!pageState.screenRunning) {
    pageState.screenRunning = true;

    final ScreenArea body = new ScreenArea(new IssueController(), request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);
  } else {

    PageSeed self = pageState.thisPage(request);
    self.setCommand(Commands.FIND);
    self.addClientEntries(pageState.getClientEntries());
    self.removeEntriesMatching("PRINT_"); // this remove ce for input defined in this page, otherwise will be duplicated

    Form f = new Form(self);
    pageState.setForm(f);
    f.start(pageContext);

    Img log = new Img(ApplicationState.getApplicationSetting(SystemConstants.PRINT_LOGO), "");

    boolean printOnSeparatePages = pageState.getEntryOrDefault("PRINT_LINES_SEPARATE_PAGES", Fields.FALSE).checkFieldValue();

%><table border="0" width="100%" align="center" cellpadding="5" cellspacing="0">
      <tr>
        <td align="left" width="90%"><%log.toHtml(pageContext);%></td>
        <td align="right"><%

          ButtonJS print = new ButtonJS("window.print();");
          print.label = "";
          print.toolTip = I18n.get("PRINT_PAGE");
          print.iconChar = "p";
          print.toHtmlInTextOnlyModality(pageContext);

        %></td>
      </tr>
    </table><%

%> <div id="printFilter" style="visibility:visible;" class="noprint">
  <table width="100%" align="center"><tr><td width="100%"> <%
  //filters


  CheckField showCh = new CheckField("PRINT_LINES_SEPARATE_PAGES", "&nbsp;", false);
  showCh.preserveOldValue = false;
  ButtonSubmit sc = new ButtonSubmit(f);
  showCh.additionalOnclickScript = sc.generateJs().toString();
  showCh.toHtmlI18n(pageContext);


  %> </td> </tr> </table> </div> <%



    Page issues = pageState.getPage();
    boolean usetypeonissues = Fields.TRUE.equalsIgnoreCase(ApplicationState.getApplicationSetting("USETYPEONISSUES"));


  if(issues != null) {

    if (!printOnSeparatePages) {
      %><table border="0" align="center" cellpadding="5" cellspacing="0" style="table-layout: fixed;width:100%" class="table dataTable"><%
    }

      boolean isFirst = true;

      for (Object o : issues.getAllElements()) {
        Issue issue = Issue.load((String) ((Object[]) o)[0]);
        if (issue == null)
          continue;

        if (printOnSeparatePages) {
          pageState.setMainObject(issue);
          %><jsp:include page="issuePrintDetail.jsp" /><%

        } else {

           if (isFirst) {
                %>
              <tr>
                <th class="tableHead">id</th>
                <th class="tableHead" style="width: 15%"><%=I18n.get("ISSUE_TASK")%></th>
                <th class="tableHead" style="width: 30%" ><%=I18n.get("ISSUE_DESCRIPTION")%></th>
                <%if(usetypeonissues){%>
                  <th class="tableHead"><%=I18n.get("ISSUE_TYPE_SHORT")%></th>
                <%}%>
                <th class="tableHead" nowrap><%=I18n.get("ISSUE_ASSIGNED_BY")%></th>
                <th class="tableHead" nowrap><%=I18n.get("ASSIGNED_TO")%></th>
                <th class="tableHead" nowrap style="width:8%"><%=I18n.get("ISSUE_CLOSE_BY")%></th>
                <th class="tableHead" style="width:5%"><%=I18n.get("ISSUE_GRAVITY_SHORT")%></th>
                <th class="tableHead" style="width:5%"><%=I18n.get("ISSUE_STATUS_SHORT")%></th>
                <th class="tableHead" style="width:5%"><%=I18n.get("ISSUE_WORKLOGS_SHORT")%></th>
              </tr><%
              }

              %><tr  class="alternate" valign="top">
              <td ><small>I#<%=issue.getMnemonicCode()%>#</small></td>
              <td ><%=JSP.w((issue.getTask()!=null ? issue.getTask().getDisplayName() :  ""))%>&nbsp;</td>
              <td > <%=JSP.w(JSP.encode(issue.getDescription()))%>&nbsp;</td>
              <%if(usetypeonissues){%>
                <td ><%=JSP.w((issue.getType()!=null ? issue.getType().getDescription() :  ""))%>&nbsp;</td>
              <%}%>
              <td ><%=JSP.w((issue.getAssignedBy()!=null ? issue.getAssignedBy().getDisplayName() :  ""))%>&nbsp;</td>
              <td ><%=JSP.w((issue.getAssignedTo()!=null ? issue.getAssignedTo().getDisplayName() :  ""))%>&nbsp;</td>
              <td ><%=JSP.w(issue.getShouldCloseBy())%>&nbsp;</td>
              <td ><%=I18n.get(issue.getGravity()+"")%>&nbsp;</td>
              <td ><%=JSP.w(issue.getStatus().getDescription())%>&nbsp;</td>
              <td  align="right"><%=(issue.getWorklogDone()>0 ? DateUtilities.getMillisInHoursMinutes(issue.getWorklogDone()) : "")%>&nbsp;</td>
              </tr><%
        }
        isFirst = false;
      }

    if(!printOnSeparatePages)
      %></table><%
    }
    f.end(pageContext);
  }
%>