<%@ page import="org.jblooming.waf.view.PageState, org.jblooming.operator.Operator, org.jblooming.oql.QueryHelper, org.jblooming.messaging.Message, org.jblooming.oql.OqlQuery, java.util.List, java.util.Date, com.twproject.operator.TeamworkOperator, org.jblooming.utilities.DateUtilities, org.hibernate.Query, org.jblooming.waf.view.PageSeed, org.jblooming.waf.html.button.ButtonLink, org.jblooming.waf.settings.I18n, org.jblooming.waf.html.button.ButtonJS, org.jblooming.utilities.JSP" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);


  long topMillis = pageState.getEntry("topMillis").longValueNoErrorNoCatchedExc();

  Operator logged = pageState.getLoggedOperator();

  String hql = "select msg from " + Message.class.getName() + " as msg where msg.toOperator.id=:ope and msg.media=:mMedia order by msg.received desc";
  QueryHelper queryH = new QueryHelper(hql);
  queryH.setParameter("ope", logged.getId());
  queryH.setParameter("mMedia", "LOG");

  String isInPage = pageState.getEntry("isInPage").stringValueNullIfEmpty();

  if (topMillis > 0) {
    queryH.addOQLClause("msg.received<:topM", "topM", new Date(topMillis));
  }

  int limit=10;

  if(JSP.ex(isInPage))
    limit = 50;

  Query query = queryH.toHql().getQuery();
  query.setMaxResults(limit);
  List<Message> messages = query.list(); //todo mettere limite


  long lastMillis=0;
  int i=1;

  if (messages.size()>0){
  for (Message message : messages) {
    if (i>=limit-1)
      break;
%>
<div id="message_<%=message.getId()%>" class="message <%=message.getReadOn()==null?"unRead":"read"%>" messageId="<%=message.getId()%>" style="<%=(JSP.ex(message.getLink())?"cursor:pointer;":"")%>" onclick="followMessageLink($(this));">
  <span class="message_operator"><%if (message.getFromOperator()!=null){
    TeamworkOperator.load(message.getFromOperator().getId()).getPerson().bricks.getAvatarImage("").toHtml(pageContext);
  }else{
      %><span class="teamworkIcon message_icon">
      <%-- TODO: SYSTEM MESSAGE TYPE --%>
      </span><%
  }%></span>
  <span class="message_date"><%=DateUtilities.dateToRelative(message.getReceived())%></span>
  <span class="message_subject " ><%=JSP.w(message.getSubject())%></span>
  <span class="message_body"><%=JSP.w(message.getMessageBody())%></span>
  <span class="message_link"><%=JSP.w(message.getLink())%></span>
</div>
<%
  if(!JSP.ex(isInPage)){
%>
<hr>
<%
    }
    lastMillis=message.getReceived().getTime();
    i++;
  }

  if (i>=limit-1){
    ButtonJS more = new ButtonJS("event.stopPropagation();loadMessageListMore($(this),"+lastMillis+","+JSP.ex(isInPage)+")");
    more.additionalCssClass="maintain";
    more.label = I18n.get("MORE");

%><div class="loadMore" ><%more.toHtmlInTextOnlyModality(pageContext);%></div><%
  }
  } else {
   %><div class="message"><span class="message_subject " ><%=I18n.get("NO_MESSAGES")%></span></div><%
  }


%>
