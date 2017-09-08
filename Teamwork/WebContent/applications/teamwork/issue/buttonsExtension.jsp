<%@ page import="org.jblooming.waf.constants.Commands, org.jblooming.waf.constants.Fields, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState" %>
<div style="text-align:center;padding:10px;">

  <%
    PageState pageState = PageState.getCurrentPageState(request);

    String cmd = pageState.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty();

    String width = "170px";
    {
      PageSeed ps = pageState.thisPage(request);
      ps.command = Commands.FIND;
      ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_ISSUES_OPENED_RECENTLY");
      ButtonLink bl = new ButtonLink(pageState.getI18n("PF_ISSUES_OPENED_RECENTLY"), ps);
      bl.width = width;
      bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
      bl.toHtml(pageContext);
    }

  %><br><%

  {
    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.FIND;
    ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_ISSUES_CLOSED_RECENTLY");
    ButtonLink bl = new ButtonLink(pageState.getI18n("PF_ISSUES_CLOSED_RECENTLY"), ps);
    bl.width = width;
    bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
    bl.toHtml(pageContext);
  }

%><br><%

  {
    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.FIND;
    ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_LONG_STANDING_ISSUES");
    ButtonLink bl = new ButtonLink(pageState.getI18n("PF_LONG_STANDING_ISSUES"), ps);
    bl.width = width;
    bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
    bl.toHtml(pageContext);
  }

%><br><%

  {
    PageSeed ps = pageState.thisPage(request);
    ps.command = Commands.FIND;
    ps.addClientEntry(Fields.FLD_FILTER_NAME, "PF_OPEN_SEVERE_ISSUES");
    ButtonLink bl = new ButtonLink(pageState.getI18n("PF_OPEN_SEVERE_ISSUES"), ps);
    bl.width = width;
    bl.hasFocus = ps.getEntry(Fields.FLD_FILTER_NAME).stringValueNullIfEmpty().equals(cmd);
    bl.toHtml(pageContext);
  }

%></div>