<%@ page import="com.twproject.operator.TeamworkOperator, com.twproject.rank.Hit, com.twproject.resource.Person, com.twproject.security.TeamworkPermissions, org.jblooming.agenda.CompanyCalendar, org.jblooming.oql.OqlQuery, org.jblooming.persistence.PersistenceHome, org.jblooming.utilities.DateUtilities, org.jblooming.waf.constants.Commands, org.jblooming.waf.html.button.ButtonImg, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.html.display.Img, org.jblooming.waf.settings.I18n, org.jblooming.waf.view.PageSeed, org.jblooming.waf.view.PageState, java.io.Serializable, java.util.Date, java.util.List" %><%

  PageState pageState = PageState.getCurrentPageState(request);

  String hql = "select sum(hit.weight), operatorId from "+ Hit.class.getName()+" as hit where hit.when>:since group by operatorId order by sum(hit.weight) desc";
  OqlQuery oql = new OqlQuery(hql);

  CompanyCalendar cc = new CompanyCalendar();
  cc.add(CompanyCalendar.MONTH, -1);
  cc.setAndGetTimeToDayStart();
  oql.getQuery().setLong("since",cc.getTime().getTime());

  List<Object[]> os = oql.list();

%><div class=" twActivity portletBox small">

  <h1><%=I18n.get("TEAMWORK_ACTIVITY")%></h1>

<table class="table" cellpadding="0" cellspacing="0">
  <tr>
    <td colspan="3"><hr></td>
  </tr>
<%
  TeamworkOperator logged = (TeamworkOperator) pageState.getLoggedOperator();

  int pos = 1;
  boolean swap=false;

  for (Object[] o : os) {

    TeamworkOperator to = (TeamworkOperator) PersistenceHome.findByPrimaryKey(TeamworkOperator.class,(Serializable)o[1]);
    if(to !=null)
    {
    if (!to.isEnabled())
      continue;
        
    Person p = to.getPerson();

     if (!(p.hasPermissionFor(logged, TeamworkPermissions.resource_canRead)))
      continue;

    int score = ((Double)o[0]).intValue();

    int llo = -1;
    if (to.getLastLoggedOn()!=null)
      llo = CompanyCalendar.getDistanceInWorkingDays(to.getLastLoggedOn(), new Date());

    String scoreName =
        (pos == 1 ? "<font color=\"#D4A017\">" : (pos == 2 ? "<font color=\"silver\">" : (pos == 3 ? "<font color=\"#9C6963\">" : ""))) +
            score +
            (pos < 4 ? "</font>" : "");

    %><tr>

      <td style="width:5%;display: none"><%=pos++%>.<%=score == 0 ? "<small>" : "" %>
        <%=score == 0 ? "</small>" : "" %>
      </td>

  <td width="40" valign="top" style="padding-top: 2px"><%

      Img personalImage = p.bricks.getAvatarImage("");
      if (personalImage != null) {
        personalImage.script = "class='face small'";

        if (p.getMyself().getId() == pageState.getLoggedOperator().getId()) {
          Person person = logged.getPerson();
          PageSeed psPhoto = pageState.pageFromRoot("resource/resourceEditor.jsp");
          psPhoto.setCommand(Commands.EDIT);
          psPhoto.setMainObjectId(person.getId());
          ButtonLink linkToResource = new ButtonLink(psPhoto);
          ButtonImg imageButton = new ButtonImg(linkToResource, personalImage);
          imageButton.toHtml(pageContext);
        } else {
          personalImage.toHtml(pageContext);
        }
      }

    %></td>
  <td valign="top">
    <%
      PageSeed ps = pageState.pageFromRoot("resource/resourceEditor.jsp");
      ps.setCommand(Commands.EDIT);
      ps.setMainObjectId(p.getId());
      ButtonLink linkToResource = new ButtonLink(ps);
      linkToResource.label = p.getDisplayName();
      linkToResource.toHtmlInTextOnlyModality(pageContext);
    %>&nbsp;<%
    p.bricks.getPopupMessage(pageState).toHtml(pageContext);
  %><br>
    <%
      if (llo == -1) {
    %>-<%
  } else if (llo > 1) {
  %><span title="<%=I18n.get("LAST_LOGIN")%> <%=llo + "(d.)"%>"><%=DateUtilities.dateAndHourToString(to.getLastLoggedOn())%></span><%
  } else {
  %><span title="<%=I18n.get("LAST_LOGIN")%>"> <%=DateUtilities.dateToHourMinutes(to.getLastLoggedOn())%></span><%
    }
  %>
  </td>
  <td valign="top" title="<%=I18n.get("TEAMWORK_CURRENT_SCORE")%>"><div class="score"><font><%=scoreName%></font></div></td>





    </tr><tr><td colspan="3"><hr></td></tr><%
  }

  }
%></table></div><%

%>
