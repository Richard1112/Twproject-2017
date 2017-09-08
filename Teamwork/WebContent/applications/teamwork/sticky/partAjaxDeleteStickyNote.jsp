<%@ page import="com.twproject.messaging.stickyNote.StickyNote, com.twproject.messaging.stickyNote.StickyNoteDrawer, com.twproject.operator.TeamworkOperator, org.jblooming.oql.OqlQuery, org.jblooming.persistence.hibernate.HibernateFactory, org.jblooming.waf.SessionState, org.jblooming.waf.view.PageState, java.util.List" %><%
  PageState pageState = PageState.getCurrentPageState(request);

  TeamworkOperator teamworkOperator = (TeamworkOperator) pageState.getLoggedOperator();
  OqlQuery oql= new OqlQuery("from "+ StickyNote.class.getName()+" as sticky where sticky.receiver=:rec and sticky.board is null");
  oql.getQuery().setEntity("rec",teamworkOperator.getPerson());
  List<StickyNote> list=oql.list();
  String command = pageState.getEntry("STICKY_CMD").stringValueNullIfEmpty();
  if("delete".equals(command)){
  for(StickyNote s : list){
    s.remove();
  }}else{
      int x = 200;
      int y = 200;
      for (StickyNote sn: list){
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
        HibernateFactory.getSession().evict(sn);
      }
  }
%>